%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 16:43
%%%-------------------------------------------------------------------
-module(dc_naive).
-author("liya").

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  collecting/2,
  state_name/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------

start_link({My_node, My_protocol, Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(My_node)++"_app"),
  log:info(" [~p] ~p created ~n",[Me]),
  timer:sleep(1500),
  {ok,Pid}=gen_fsm:start_link({local, Me}, ?MODULE, {Me, My_protocol,My_node,Meters}, []),
  Pid.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%--------------------------------------------------------------------
init({Me, My_protocol,My_node,Meters}) ->
  Hand_shake =hand_shake(Me,My_protocol,1),
  case Hand_shake of
    ready ->
      log:info(" [~p] ~p initialized~n", [Me]),
      Nrs = Meters,
      Rd = random_elements (Nrs),                         % 1/4+9c
      Nrs1 = delete_elements (Nrs, Rd),                   % 1/3+10
      ets:new(mr_ets,[ordered_set, named_table, public]), % create M
      _Ok = send_dreq(My_protocol,Rd,0),                    % 1/11
      log:info(" [~p] first dreq sent, Rd are ~p~n",[Rd]),
      Timerpid = erlang:spawn(?MODULE, timer, [Me]),        % 1/12
    %  log:info(" [~p] AFTER TIMER SPAWN : ~w~n",[Timerpid]),
      {ok, collecting, {Me, My_protocol,My_node,Meters,Nrs1,Rd,0,Timerpid}};

    {terminate, Reason} -> log:critical("handshake with ~p failed with message:
     ~p~n", [My_protocol,Reason]),
      {stop,{handshake_failure,Reason}}
  end.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @end
%%--------------------------------------------------------------------
collecting({drep,To,Data,Seq},{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  if To =/= My_node ->
    log:err("dc recived drep with destination address of: ~p, ignoring~n",[To]),
    {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Rd,Sn,Timerpid}} ;
    true ->
      {V,_} = lists:last(Data),                          % 2/10
      case Seq of
        Seq when Seq>Sn ->
          log:err("received drep from ~p in state collecting with higher seq of
      ~p,ignore.~n state data: Nrs:~p, Rd:~p,Sn:~p~n",
            [V,Seq,Nrs,Rd,Sn]),
          next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Rd,Sn,Timerpid};
        Seq when Seq<Sn ->
          % the seq num of the drep is lower than expected -> error,check
          log:err("received drep from ~p in state collecting with lower seq of
      ~p,ignore.~n state data: Nrs:~p, Rd:~p,, Sn:~p~n",
            [V,Seq,Nrs,Rd,Sn]),
          _Ok = check_reading_and_log_time(),       %%todo
          {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Rd,Sn,Timerpid}};
        Seq when Seq==Sn ->
          log:debug("received drep from: ~p, in state collecting ~n state data: Nrs:
       ~p,Rd = ~p, Sn: ~p~n", [V,Nrs,Rd,Sn]),
          Nodes = extract_nodes_from_drep([], Data),
          Nrs1 = lists:subtract(Nrs, Nodes),                 % 2/15
          Rd2 = lists:subtract(Rd,Nodes),
          _Ok = ets:insert(mr_ets,Data),
          if Rd2 == [] ->  gen_fsm:send_event(Me, rd_empty),
            {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Sn,
              Timerpid}};
            true -> {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Sn,
              Timerpid}}
          end
      end
  end;

collecting(rd_empty,{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  log:debug("received rd_empy event in state collecting,~n State data:
    Nrs: ~p, Rd: ~p, Sn: ~p~n" , [Nrs,Rd,Sn]),
  Nrs1 =lists:usort(lists:umerge(Nrs ,Rd)),
  if
    Nrs1 == []->
      log:info(" [~p] ====== FINISHED ROUND ~p of collecting, preparing for next round =======~n",[Sn]),
      Timerpid! stop,
      Sn1 = Sn+1,
      Nrs2 = Meters,                                 % 2/4
      Rd3 = random_elements (Nrs2),                         % 1/4+9c
      Nrs3 = delete_elements (Nrs2, Rd3),                                      % 2/5
      log:info(" [~p] sending dreq to: ~p with sn ~p~n", [Rd3,Sn1]),
      _Ok = send_dreq(My_protocol,Rd3,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_protocol,My_node,Meters,Nrs3,Rd3,Sn1,Timerpid1}};
    true ->
      log:debug("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      Rd1 = random_elements (Nrs1),                  % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),            % 1/10
      log:info(" [~p] sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_protocol,Rd1,Sn),             % 1/11
      Timerpid!restart,                              % 1/12
      {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Sn,Timerpid}}
  end;


%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(state_name(Event :: term(), From :: {pid(), term()},
    State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{}} |
  {reply, Reply, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewState :: #state{}} |
  {stop, Reason :: normal | term(), Reply :: term(),
    NewState :: #state{}}).
state_name(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), StateName :: atom(),
    StateData :: #state{}) ->
  {next_state, NextStateName :: atom(), NewStateData :: #state{}} |
  {next_state, NextStateName :: atom(), NewStateData :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewStateData :: #state{}}).
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()},
    StateName :: atom(), StateData :: term()) ->
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term()} |
  {reply, Reply :: term(), NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewStateData :: term()} |
  {stop, Reason :: term(), NewStateData :: term()}).
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: term(), StateName :: atom(),
    StateData :: term()) ->
  {next_state, NextStateName :: atom(), NewStateData :: term()} |
  {next_state, NextStateName :: atom(), NewStateData :: term(),
    timeout() | hibernate} |
  {stop, Reason :: normal | term(), NewStateData :: term()}).
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: normal | shutdown | {shutdown, term()}
| term(), StateName :: atom(), StateData :: term()) -> term()).
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, StateName :: atom(),
    StateData :: #state{}, Extra :: term()) ->
  {ok, NextStateName :: atom(), NewStateData :: #state{}}).
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
