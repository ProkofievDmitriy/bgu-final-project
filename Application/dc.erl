%%%------------------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2017 11:43
%%%------------------------------------------------------------------------------
-module(dc).
-author("liya").

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
  timer/1,
  discovering/2,
  collecting/2,
  state_name/3,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4,
  my_rand/1,
  random_indexes/3]).

-include("app_macros.hrl").

-record(state, {}).

%%%==============================================================================
%%% API
%%%==============================================================================

%%-------------------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%-------------------------------------------------------------------------------


%% TODO: update protocol interface to Dima: he starts app with the properties bellow.
start_link(Me, My_server,Meters) ->
  log:info("~p created ~n",[Me]),
  {ok,Pid}=gen_fsm:start_link({local, Me}, ?MODULE, {Me, My_server,Meters}, []),
  Pid.

%%%=================================================================================
%%% gen_fsm callbacks
%%%=================================================================================

%%----------------------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @end
%%----------------------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).




%% Me -  self
%% My_server - the gen server of the protocol in tha same node (DC)
%% Meters - list of names of the other nodes

%% TODO add a handshake before doing any activity (gen_server:call),do the checking inside the function and if failed call handshake over and over again.

init({Me, My_server,Meters}) ->
  Hand_shake =hand_shake(Me,My_server,1),
  case Hand_shake of
    ready ->
      io:format("~p initialized~n", [Me]),
      Nrs = Meters,
      Rd = random_elements (Nrs),                         % 1/4+9c
      Nrs1 = delete_elements (Nrs, Rd),                   % 1/3+10
      ets:new(mr_ets,[ordered_set, named_table, public]), % create M
      _Ok = send_dreq(My_server,Rd,0),                    % 1/11
      io:format("first dreq sent, Rd are ~p~n",[Rd]),
      Timerpid = erlang:spawn(?MODULE,timer,[Me]),        % 1/12
      {ok, discovering, {Me, My_server,Meters,Nrs1,Rd,[],0,Timerpid}};
    {terminate, Reason} -> log:critical("handshake with ~p failed with message:
     ~p~n", [My_server,Reason]),
      {stop,{handshake_failure,Reason}}
  end.


%%----------------------------------------------------------------------------------
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
-spec(discovering(Event :: term(), State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

%%===================================================================================
%%=================================  PHASE 1 ========================================
%%===================================================================================

%% phase 1 of AMR - AM
discovering({drep,?DC_NODE,Data,Seq},{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
 % ok, drep received with destination address of dc
  {V,_} = lists:last(Data),
  case Seq of
  % the seq num of the drep is higher than expected -> error, ignore(maybe not?)
    Seq when Seq>Sn ->
      log:err("received drep from ~p in state discovering with higher seq of
      ~p,ignore.~n state data: Nrs:~p, Rd:~p, Ter:~p, Sn:~p~n",
       [V,Seq,Nrs,Rd,Ter,Sn]),
      {next_state, discovering, {Me,My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
    Seq when Seq<Sn ->
      % the seq num of the drep is lower than expected -> error,check
      log:err("received drep from ~p in state discovering with lower seq of
      ~p,ignore.~n state data: Nrs:~p, Rd:~p, Ter:~p, Sn:~p~n",
       [V,Seq,Nrs,Rd,Ter,Sn]),
      _Ok = check_reading_and_log_time(),       %%todo
      {next_state, discovering, {Me,My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
    Seq when Seq==Sn ->
     log:debug("received drep from: ~p, in state discovering ~n state data:
      Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n",
      [V,Nrs,Rd,Ter,Sn]),
      Rd1 = lists:delete(V,Rd),                         % 1/15
      Ter1 = lists:umerge([Ter,[V]]),                   % 1/16-18
      Nodes = extract_nodes_from_drep([], Data),
      log:debug("extarcting nodes from drep: ~p~n",[Nodes]),
      Nodes1 = lists:delete(V,Nodes),
      Ter2 = lists:subtract(Ter1,Nodes1),               % 1/19                    ,
      Nrs1 = lists:subtract(Nrs, Nodes),                % 1/21
      Rd2 = lists:subtract(Rd1, Nodes),                 % 1/21
      _Ok = ets:insert(mr_ets,Data),
      log:debug("Rd2 is now ~p~n",[Rd2]),
      if Rd2 == [] ->  gen_fsm:send_event(Me, rd_empty),
        {next_state, discovering, {Me,My_server,Meters,Nrs1,Rd2,Ter2,Sn,
         Timerpid}};
        true -> {next_state, discovering, {Me,My_server,Meters,Nrs1,Rd2,Ter2,Sn,
         Timerpid}}
      end
  end;

%% received drep with other destination address then dc
discovering({drep,To,_Data,_Seq},{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
 log:err("dc recived drep with dest address of: ~p, in state discovering ignoring~n", [To]),
 {next_state, discovering, {Me,My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}};


%% Rd = [] , prepare for another iteration of external loop
discovering(rd_empty,{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  log:debug("received rd_empy event in state discovering,~n State data:
   Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n" , [Nrs,Rd,Ter,Sn]),
  Nrs1 = lists:usort(lists:umerge(Nrs,Rd)),                    % 1/23
  log:debug("Nrs1 is ~p~n",[Nrs1]),
  case  Nrs1 of
    [] ->        %% external loop terminated, go ro phase 2
      log:info("=========FINISHED reading sems in phase1, preparing for PHASE 2 ========~n"),
      Timerpid!stop,
      Sn1=Sn+1,                                      % 2/3
      Nrs2 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      log:info("sending dreq to terminals: ~p with sn ~p~n", [Ter8,Sn1]),
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_server,Meters,Nrs2,Ter8,Ter1,Sn1,Timerpid1}};
    Nrs1  ->              %% otherwise prepare for next iteration
      log:debug("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      Rd1 = random_elements (Nrs1),                  % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),            % 1/10
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_server,Rd1,Sn),             % 1/11
      Timerpid!restart,                              % 1/12
      {next_state, discovering, {Me,My_server,Meters,Nrs2,Rd1,Ter,Sn,Timerpid}}
  end;

%% timeout elapsed, prepare for another iteration of external loop
discovering(timeout,{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  log:debug("received timeout event in state discovering, State data:~n
   Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n" , [Nrs,Rd,Ter,Sn]),
  Nrs1 = lists:usort(lists:umerge(Nrs,Rd)),                       % 1/23
  if Nrs1 == [] ->        %% external loop terminated, go ro phase 2
      log:info("=========FINISHED reading sems in phase1, preparing for PHASE 2 ========~n"),
      Timerpid!stop,
      Sn1=Sn+1,                                      % 2/3
      Nrs1 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      log:info("sending dreq to: ~p with sn ~p~n", [Ter8,Sn1]),
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_server,Meters,Nrs1,Ter8,Ter1,Sn1,Timerpid1}};
    true ->              %% otherwise prepare for next iteration
      log:debug("didnt receive all requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      Rd1 = random_elements (Nrs1),                   % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),             % 1/10
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_server,Rd1,Sn),              % 1/11
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),   % 1/12
      {next_state, discovering, {Me,My_server,Meters,Nrs2,Rd1,Ter,Sn,Timerpid1}}
  end;

discovering( Event , State_data) ->
  log:err("UNEXPECTED EVENT: ~p in state discovering with state data: ~p~n", [Event,State_data]),
  {next_state, discovering, State_data}.



%%===================================================================================
%%=================================  PHASE 2 ========================================
%%===================================================================================

%%TODO handle info

%%TODO update drep format in phase 2

collecting({drep,?DC_NODE,Data,Seq},{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  {V,_} = lists:last(Data),                          % 2/10
  case Seq of
    Seq when Seq>Sn ->
      log:err("received drep from ~p in state collecting with higher seq of
      ~p,ignore.~n state data: Nrs:~p, Ter8:~p, Ter:~p, Sn:~p~n",
      [V,Seq,Nrs,Ter8,Ter,Sn]),
      next_state, collecting, {Me,My_server,Meters,Nrs,Ter8,Ter,Sn,Timerpid};
    Seq when Seq<Sn ->
      % the seq num of the drep is lower than expected -> error,check
      log:err("received drep from ~p in state collecting with lower seq of
      ~p,ignore.~n state data: Nrs:~p, Ter8:~p, Ter:~p, Sn:~p~n",
       [V,Seq,Nrs,Ter8,Ter,Sn]),
      _Ok = check_reading_and_log_time(),       %%todo
      {next_state, collecting, {Me,My_server,Meters,Nrs,Ter8,Ter,Sn,Timerpid}};
    Seq when Seq==Sn ->
      log:debug("received drep from: ~p, in state collecting ~n state data: Nrs:
       ~p,Ter* = ~p, Ter: ~p, Sn: ~p~n", [V,Nrs,Ter8,Ter,Sn]),
      Ter81 = lists:delete(V,Ter8),
      Ter1 = lists:umerge([Ter,[V]]),                    % 2/11
      Nodes = extract_nodes_from_drep([], Data),
      Nodes1 = lists:delete(V,Nodes),
      Ter2 = lists:subtract(Ter1,Nodes1),                % 2/16
      Nrs1 = lists:subtract(Nrs, Nodes),                 % 2/15
      Ter82 = lists:subtract(Ter81,Nodes1),
      _Ok = ets:insert(mr_ets,Data),
      if Ter82 == [] ->  gen_fsm:send_event(Me, ter8_empty),
        {next_state, collecting, {Me,My_server,Meters,Nrs1,Ter82,Ter2,Sn,
         Timerpid}};
        true -> {next_state, collecting, {Me,My_server,Meters,Nrs1,Ter82,Ter2,Sn,
         Timerpid}}
      end
    end;


 collecting({drep,To,_Data,_Seq},{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
   log:err("dc recived drep with destination address of: ~p, ignoring~n",[To]),
   {next_state, collecting, {Me,My_server,Meters, Nrs, Ter8,Ter,Sn,Timerpid}} ;


collecting(ter8_empty,{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
   log:debug("received ter8_empy event in state collecting,~n State data:
    Nrs: ~p, Ter8: ~p, Ter: ~p, Sn: ~p~n" , [Nrs,Ter8,Ter,Sn]),
  Ter81 =lists:usort(lists:umerge(Nrs ,Ter8)),
  if
    Nrs == []->
      log:info("====== FINISHED ROUND ~p of collecting, preparing for next round =======~n",[Sn]),
      Timerpid! stop,
      Sn1 = Sn+1,
      Nrs1 = Meters,                                 % 2/4
      Ter82 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
       log:info("sending dreq to: ~p with sn ~p~n", [Ter82,Sn1]),
      _Ok = send_dreq(My_server,Ter82,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_server,Meters,Nrs1,Ter82,Ter1,Sn1,Timerpid1}};
    true ->
      log:debug("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      log:info("sending dreq to: ~p with sn ~p~n", [Ter81,Sn]),
      _Ok = send_dreq(My_server,Ter81,Sn),           % 2/7
      Timerpid ! restart,
      {next_state, collecting, {Me,My_server,Meters, Nrs, Ter81,Ter,Sn,Timerpid}}
  end;

collecting(timeout,{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  log:debug("received TIMEOUT event in state collecting, State data:~n
   Nrs: ~p, Ter8: ~p, Ter: ~p, Sn: ~p~n" , [Nrs,Ter8,Ter,Sn]),
  Ter81 = lists:usort(lists:umerge(Nrs ,Ter8)),
  if
    Nrs == []->
      log:info("===== FINISHED ROUND ~p of collecting, preparing for next round~====== n",[Sn]),
      Timerpid! stop,
      Sn1 = Sn+1,
      Nrs1 = Meters,                                 % 2/4
      Ter82 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      log:info("sending dreq to: ~p with sn ~p~n", [Ter82,Sn1]),
      _Ok = send_dreq(My_server,Ter82,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_server,Meters,Nrs1,Ter82,Ter1,Sn1,Timerpid1}};
    true ->
      log:debug("didnt receive all requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      log:info("sending dreq to: ~p with sn ~p~n", [Ter81,Sn]),
      _Ok = send_dreq(My_server,Ter81,Sn),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting, {Me,My_server,Meters, Nrs, Ter81,Ter,Sn,Timerpid1}}
  end.



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

handle_info(Info, StateName, State) ->
  log:err(" ~p received UNEXPECTED MESSAGE ~p in state ~ with data ~p",[self(),Info,StateName,State]),
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
terminate(Reason, StateName, {Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  Timerpid! stop,
  log:info("terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [Reason,StateName,{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}]),
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


hand_shake(Me,My_server,Times) ->
  case ?TEST_MODE of
      local ->
       My_server! {app_handshake, {Me,dc}},  %% format: {pid/name , type(dc/sem)}
         receive
           ok -> ready;
           Err-> case Times of
                           Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                             log:err("handshake failed with err ~p, on try number: ~p , trying again~n",[Err,Times]),
                             hand_shake(Me,My_server,Times+1);
                           Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                             log:err("handshake failed with err ~p, on try number: ~p , TERMINATING~n",[Err,Times]),
                             {terminate, Err}
                         end
           after ?HAND_SHAKE_TIMEOUT -> case Times of
                                          Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                                            log:err("handshake timeout on try number: ~p , trying again~n",[Times]),
                                            hand_shake(Me,My_server,Times+1);
                                          Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                                            log:err("handshake timeout on try number: ~p , TERMINATING~n",[Times]),
                                            {terminate, timeout}
                                        end
         end;
    integrated -> ok
      %% translate local to OTP
      end.




my_rand(Number) ->
  %rand:seed(erlang:timestamp()),
  {A,B,C} = erlang:timestamp(),
  {RandomNumber, _Seed} = rand:uniform_s(Number, rand:seed(exs1024,{A,B,C})),
  RandomNumber.


random_indexes(_,0,List) -> List;
random_indexes(Length,Iteration,List) ->
  Index = my_rand(Length),
  Bool = lists:member(Index, List) ,
  if Bool -> random_indexes(Length,Iteration,List);
    true -> random_indexes(Length,Iteration-1,[Index|List])
  end.

selected_elements(_,[],New) -> New;
selected_elements(List,[H|T],New)->
  selected_elements(List,T,[lists:nth(H,List)|New]).

random_elements(List) ->
  Length = trunc(math:sqrt(lists:flatlength(List))),
  Indexes = random_indexes(erlang:length(List),Length,[]),
  selected_elements(List,Indexes,[]).

delete_elements(List,[])->List;
delete_elements(List,[H|T])->
  delete_elements(lists:delete(H,List),T).

%%TODO update regular message send to a gen_server call

send_dreq(_,[],_) -> ok;
send_dreq(My_server, [H|T], Seq) ->
  case ?TEST_MODE of
    local ->
      log:debug("sending dreq to: ~p with sequence ~p~n", [H,Seq]),
      My_server ! {dreq, H, Seq},
      send_dreq(My_server, T, Seq);
    integrated ->
      % translate local to OTP BEHAVIOR
      ok
  end.

timer(Me) ->
  receive
    stop -> erlang:exit(stopped);
    restart -> timer(Me)
  after ?TIMEOUT -> gen_fsm:send_event(Me,timeout)
  end.


extract_nodes_from_drep(List,[]) -> List;
extract_nodes_from_drep(List,[{Node,_}|T]) ->
  extract_nodes_from_drep([Node|List],T).

 %%TODO 
check_reading_and_log_time() ->
  ok.




































