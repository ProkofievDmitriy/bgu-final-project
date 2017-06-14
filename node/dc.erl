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
-export([start_link/1]).
-compile(export_all).
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

start_link({My_node, My_protocol, Meters}) ->
   Me = erlang:list_to_atom(atom_to_list(My_node)++"_app"),
   log:info("~p created ~n",[Me]),
   % timer:sleep(1500),
  {ok,Pid}=gen_fsm:start_link({local, Me}, ?MODULE, {Me, My_protocol,My_node,Meters}, []),
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
%% My_protocol - the gen server of the protocol in tha same node (DC)
%% My_node - the address of the node i'm sitting in
%% Meters - list of names of the other nodes


init({Me, My_protocol,My_node,Meters}) ->
  Hand_shake =hand_shake(Me,My_protocol,1),
  case Hand_shake of
    ready ->
      log:info("~p initialized~n", [Me]),
      Nrs = Meters,
      Rd = random_elements (Nrs),                         % 1/4+9c
      Nrs1 = delete_elements (Nrs, Rd),                   % 1/3+10
      ets:new(mr_ets,[ordered_set, named_table, public]), % create M
      _Ok = send_dreq(My_protocol,Rd,0),                    % 1/11
      log:info("first dreq sent, Rd are ~p~n",[Rd]),
      Timerpid = erlang:spawn(?MODULE, timer, [Me]),        % 1/12
      {ok, discovering, {Me, My_protocol,My_node,Meters,Nrs1,Rd,[],0,Timerpid}};

    {terminate, Reason} -> log:critical("handshake with ~p failed with message:
     ~p~n", [My_protocol,Reason]),
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


%% TODO 1. implement internal functions. 2. do the same for collecting. 3. do the same for sem. (maybe a different module?)
%%discovering({received_message, Bit_string},{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
%%  log:info("received bit string: ~p~n", [Bit_string]),
%%  <<Type:1, To_b:?NODE_BITS, Seq_b:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
%%  To_n = erlang:binary_to_integer(bitstring_to_binary(To_b)),
%%  To = extract_name(To_n),
%%  Seq = erlang:binary_to_integer(bitstring_to_binary(Seq_b)),
%%  case Type of
%%    ?DREQ_BIT -> gen_fsm:send_event(Me, {dreq,To,Seq}),
%%      {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}} ;
%%    ?DREP_BIT ->
%%      Data_size = bit_size(Data_b),
%%      Entry_size = ?NODE_BITS+?READING_BITS,
%%      if Data_size rem Entry_size =/= 0 ->
%%        log:err("received invalid data of size ~p, dropping drep~n",[Data_size]),
%%        {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
%%      true ->
%%          Data = bit_to_data(Data_b,[]),
%%          gen_fsm:send_event(Me, {drep, To,Data,Seq}),
%%          {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}}
%%      end
%%  end;


discovering({received_message, Bit_string},{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  log:info("received bit string: ~p~n", [Bit_string]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
 % To_n = erlang:binary_to_integer(bitstring_to_binary(To_b)),
  To = extract_name(To_n),
  %Seq = erlang:binary_to_integer(bitstring_to_binary(Seq_b)),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(Me, {dreq,To,Seq}),
      {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}} ;
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      Entry_size = ?NODE_BITS+?READING_BITS,
      if Data_size rem Entry_size =/= 0 ->
        log:err("received invalid data of size ~p, dropping drep~n",[Data_size]),
        {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
        true ->
          Data = bit_to_data(Data_b,[]),
          gen_fsm:send_event(Me, {drep, To,Data,Seq}),
          {next_state, discovering,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}}
      end
  end;

%% phase 1 of AMR - AM
discovering({drep,To,Data,Seq},{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
    log:debug("enter discovering : ~w~n", [self()]) ,
  if To =/=My_node ->
    log:err("dc recived drep with dest address of: ~p, in state discovering ignoring~n", [To]),
    {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
    true ->
 % ok, drep received with destination address of dc
  {V,_} = lists:last(Data),
%%  case Seq of
%%  % the seq num of the drep is higher than expected -> error, ignore(maybe not?)
%%    Seq when Seq>Sn ->
%%      log:err("received drep from ~p in state discovering with higher seq of
%%      ~p,ignore.~n state data: Nrs:~p, Rd:~p, Ter:~p, Sn:~p~n",
%%       [V,Seq,Nrs,Rd,Ter,Sn]),
%%      {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
%%    Seq when Seq<Sn ->
%%      % the seq num of the drep is lower than expected -> error,check
%%      log:err("received drep from ~p in state discovering with lower seq of
%%      ~p,ignore.~n state data: Nrs:~p, Rd:~p, Ter:~p, Sn:~p~n",
%%       [V,Seq,Nrs,Rd,Ter,Sn]),
%%      _Ok = check_reading_and_log_time(),       %%todo
%%      {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}};
%%    Seq when Seq==Sn ->
     log:info("received drep from: ~p, with Seq: ~p in state discovering ~n state data:
      Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p,  ~n",
      [V,Seq,Nrs,Rd,Ter,Sn]),
      Rd1 = lists:delete(V,Rd),                         % 1/15
      Ter1 = lists:umerge([Ter,[V]]),                   % 1/16-18
      Nodes = extract_nodes_from_drep([], Data),
      log:info("extarcting nodes from drep: ~p~n",[Nodes]),
      Nodes1 = lists:delete(V,Nodes),
      Ter2 = lists:subtract(Ter1,Nodes1),               % 1/19                    ,
      Nrs1 = lists:subtract(Nrs, Nodes),                % 1/21
      Rd2 = lists:subtract(Rd1, Nodes),                 % 1/21
      _Ok = ets:insert(mr_ets,Data),
      log:debug("Rd2 is now ~p~n",[Rd2]),
      if Rd2 == [] ->  gen_fsm:send_event(Me, rd_empty),
        {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Ter2,Sn,
         Timerpid}};
        true -> {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Ter2,Sn,
         Timerpid}}
      end
%%    end
  end;

%% Rd = [] , prepare for another iteration of external loop
discovering(rd_empty,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
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
      _Ok = send_dreq(My_protocol,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_protocol,My_node,Meters,Nrs2,Ter8,Ter1,Sn1,Timerpid1}};
    Nrs1  ->              %% otherwise prepare for next iteration
      log:info("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      Rd1 = random_elements (Nrs1),                  % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),            % 1/10
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_protocol,Rd1,Sn),             % 1/11
      Timerpid!restart,                              % 1/12
      {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Ter,Sn,Timerpid}}
  end;

%% timeout elapsed, prepare for another iteration of external loop
discovering(timeout,{Me, My_protocol,My_node,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  log:debug("received timeout event in state discovering, State data:~n
   Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n" , [Nrs,Rd,Ter,Sn]),
  Nrs1 = lists:usort(lists:umerge(Nrs,Rd)),                       % 1/23
  if Nrs1 == [] ->        %% external loop terminated, go ro phase 2
      log:info("=========FINISHED reading sems in phase1, preparing for PHASE 2 ========~n"),
      Timerpid!stop,
      Sn1=Sn+1,                                      % 2/3
      Nrs2 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      log:info("sending dreq to terminals: ~p with sn ~p~n", [Ter8,Sn1]),
      _Ok = send_dreq(My_protocol,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_protocol,My_node,Meters,Nrs2,Ter8,Ter1,Sn1,Timerpid1}};
    true ->              %% otherwise prepare for next iteration
      log:info("didnt receive all requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      Rd1 = random_elements (Nrs1),                   % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),             % 1/10
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_protocol,Rd1,Sn),              % 1/11
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),   % 1/12
      {next_state, discovering, {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Ter,Sn,Timerpid1}}
  end;

discovering( Event , State_data) ->
  log:err("UNEXPECTED EVENT: ~p in state discovering with state data: ~p~n", [Event,State_data]),
  {next_state, discovering, State_data}.

%%===================================================================================
%%=================================  PHASE 2 ========================================
%%===================================================================================


collecting({received_message, Bit_string},{Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  log:info("received bit string: ~p~n", [Bit_string]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
 % To_n = erlang:binary_to_integer(bitstring_to_binary(To_b)),
  To = extract_name(To_n),
  %Seq = erlang:binary_to_integer(bitstring_to_binary(Seq_b)),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(Me, {dreq,To,Seq}),
     {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter8,Ter,Sn,Timerpid}} ;
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      Entry_size = ?NODE_BITS+?READING_BITS,
      if Data_size rem Entry_size =/= 0 ->
        log:err("received invalid data of size ~p, dropping drep~n",[Data_size]),
        {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter8,Ter,Sn,Timerpid}} ;
        true ->
          Data = bit_to_data(Data_b,[]),
          gen_fsm:send_event(Me, {drep, To,Data,Seq}),
          {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter8,Ter,Sn,Timerpid}}
      end
  end;



collecting({drep,To,Data,Seq},{Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  if To =/= My_node ->
    log:err("dc received drep with destination address of: ~p, ignoring~n",[To]),
    {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter8,Ter,Sn,Timerpid}} ;
    true ->
  {V,_} = lists:last(Data),                          % 2/10
%%  case Seq of
%%    Seq when Seq>Sn ->
%%      log:err("received drep from ~p in state collecting with higher seq of
%%      ~p,ignore.~n state data: Nrs:~p, Ter8:~p, Ter:~p, Sn:~p~n",
%%      [V,Seq,Nrs,Ter8,Ter,Sn]),
%%      next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Ter8,Ter,Sn,Timerpid};
%%    Seq when Seq<Sn ->
%%      % the seq num of the drep is lower than expected -> error,check
%%      log:err("received drep from ~p in state collecting with lower seq of
%%      ~p,ignore.~n state data: Nrs:~p, Ter8:~p, Ter:~p, Sn:~p~n",
%%       [V,Seq,Nrs,Ter8,Ter,Sn]),
%%      _Ok = check_reading_and_log_time(),       %%todo
%%      {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Ter8,Ter,Sn,Timerpid}};
%%    Seq when Seq==Sn ->
      log:info("received drep from: ~p, with Seq: ~p in state collecting ~n state data: Nrs:
       ~p,Ter* = ~p, Ter: ~p, Sn: ~p~n", [V,Seq,Nrs,Ter8,Ter,Sn]),
      Ter81 = lists:delete(V,Ter8),
      Ter1 = lists:umerge([Ter,[V]]),                    % 2/11
      Nodes = extract_nodes_from_drep([], Data),
      Nodes1 = lists:delete(V,Nodes),
      Ter2 = lists:subtract(Ter1,Nodes1),                % 2/16
      Nrs1 = lists:subtract(Nrs, Nodes),                 % 2/15
      Ter82 = lists:subtract(Ter81,Nodes1),
      _Ok = ets:insert(mr_ets,Data),
      if Ter82 == [] ->  gen_fsm:send_event(Me, ter8_empty),
        {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Ter82,Ter2,Sn,
         Timerpid}};
        true -> {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Ter82,Ter2,Sn,
         Timerpid}}
%%      end
   end
 end;


collecting(ter8_empty,{Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
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
       log:info("sending dreq to terminals: ~p with sn ~p~n", [Ter82,Sn1]),
      _Ok = send_dreq(My_protocol,Ter82,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_protocol,My_node,Meters,Nrs1,Ter82,Ter1,Sn1,Timerpid1}};
    true ->
      log:debug("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      log:info("sending dreq to: ~p with sn ~p~n", [Ter81,Sn]),
      _Ok = send_dreq(My_protocol,Ter81,Sn),           % 2/7
      Timerpid ! restart,
      {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter81,Ter,Sn,Timerpid}}
  end;

collecting(timeout,{Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
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
      _Ok = send_dreq(My_protocol,Ter82,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_protocol,My_node,Meters,Nrs1,Ter82,Ter1,Sn1,Timerpid1}};
    true ->
      log:debug("didnt receive all requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      log:info("sending dreq to: ~p with sn ~p~n", [Ter81,Sn]),
      _Ok = send_dreq(My_protocol,Ter81,Sn),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Ter81,Ter,Sn,Timerpid1}}
  end;

collecting( Event , State_data) ->
  log:err("UNEXPECTED EVENT: ~p in state collecting with state data: ~p~n", [Event,State_data]),
  {next_state, collecting, State_data}.


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
terminate(Reason, StateName, {Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  Timerpid! stop,
  log:info("terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [Reason,StateName,{Me,My_protocol,My_node,Meters,Nrs, Ter8, Ter, Sn, Timerpid}]),
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


hand_shake(Me,My_protocol,Times) ->
  case ?TEST_MODE of
      local ->
       My_protocol! {app_handshake, {Me,dc}},  %% format: {pid/name , type(dc/sem)}
         receive
           ok -> ready;
           Err-> case Times of
                           Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                             log:err("handshake failed with err ~p, on try number: ~p , trying again~n",[Err,Times]),
                             hand_shake(Me,My_protocol,Times+1);
                           Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                             log:err("handshake failed with err ~p, on try number: ~p , TERMINATING~n",[Err,Times]),
                             {terminate, Err}
                         end
           after ?HAND_SHAKE_TIMEOUT -> case Times of
                                          Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                                            log:err("handshake timeout on try number: ~p , trying again~n",[Times]),
                                            hand_shake(Me,My_protocol,Times+1);
                                          Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                                            log:err("handshake timeout on try number: ~p , TERMINATING~n",[Times]),
                                            {terminate, timeout}
                                        end
         end;
    integrated ->
       Reply =(catch protocol_interface:hand_shake(self(), ?HAND_SHAKE_TIMEOUT))     ,
          case Reply of
            ok -> ready;
            {'EXIT',{timeout,{gen_server,call,_}}} ->
                     case Times of
                       Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                         log:err("handshake timeout on try number: ~p , trying again~n",[Times]),
                         hand_shake(Me,My_protocol,Times+1);
                       Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                         log:err("handshake timeout on try number: ~p , TERMINATING~n",[Times]),
                         {terminate, timeout}
                     end;
             Err->
                     case Times of
                                   Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                                     log:err("handshake failed with err ~p, on try number: ~p , trying again~n",[Err,Times]),
                                     hand_shake(Me,My_protocol,Times+1);
                                   Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                                     log:err("handshake failed with err ~p, on try number: ~p , TERMINATING~n",[Err,Times]),
                                     {terminate, Err}
                     end
          end
 end.



 get_current_millis() ->
   {Mega, Sec, Micro} = os:timestamp(),
   (Mega*1000000 + Sec)*1000 + round(Micro/1000).

my_rand(Number) ->
%%  rand:seed(erlang:timestamp()),
%% {A,B,C} = erlang:timestamp(),
%%   {A,B,C} = erlang:now(),
%%   {RandomNumber, _Seed} = random:uniform_s(Number, random:seed(exs1024,{A,B,C})),

%%  RandomNumber.
  RandomNumber = erlang:phash2(get_current_millis()) rem Number,
  log:debug("randomizing ~p~n", [RandomNumber]),
  case RandomNumber of
      0 -> 1;
      _ -> RandomNumber
  end.


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


send_dreq(_,[],_) -> ok;
send_dreq(My_protocol, [H|T], Seq) ->
  case ?TEST_MODE of
    local ->
      log:debug("sending dreq to: ~p with sequence ~p~n", [H,Seq]),
      Bit_message = message_to_bit ({dreq,H,Seq}),
      My_protocol ! Bit_message,
      send_dreq(My_protocol, T, Seq);

    integrated ->
      log:debug("sending dreq to: ~p with sequence ~p~n", [H,Seq]) ,
    %   Reply = (catch gen_server:call(My_protocol, {dreq, H, Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),
      Bit_message = message_to_bit ({dreq,H,Seq}),
      log:debug(" sending bit message: ~p~n", [Bit_message]),
      Reply = protocol_interface:send_data_request(H, Bit_message),
      case Reply of
        {ok, sent} ->
               send_dreq(My_protocol, T, Seq);
        {error, timeout_exceeded} ->  send_dreq(My_protocol, T, Seq);
        Err -> log:critical("error in gen_server:call in send_dreq : ~p~n",[Err])

      end


  end.


message_to_bit({dreq,To,Seq}) ->
  Type_b = <<?DREQ_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring>>;

message_to_bit({drep,To,Data,Seq})->
  Data_b = data_to_bits(Data,<<>>),
  Type_b = <<?DREP_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring, Data_b/bitstring>>.



%%bit_to_data(<<>>, List) -> List;
%%bit_to_data(Data_b, List) ->
%%  <<Meter_b:?NODE_BITS,Reading_b:?READING_BITS, Tail/bitstring>> = Data_b,
%%  Meter_n = erlang:binary_to_integer(bitstring_to_binary(Meter_b),2),
%%  Meter = extract_name(Meter_n),
%%  Reading = erlang:binary_to_integer(bitstring_to_binary(Reading_b)),
%%  bit_to_data(Tail,[{Meter,Reading}|List]).

bit_to_data(<<>>, List) -> List;
bit_to_data(Data_b, List) ->
  <<Meter_b:?NODE_BITS,Reading:?READING_BITS, Tail/bitstring>> = Data_b,
  Meter = extract_name(Meter_b),
  bit_to_data(Tail,[{Meter,Reading}|List]).



%%% readings order will be reversed at the end of this -> at the receiving side will be reversed back.
data_to_bits([],String) ->String;
data_to_bits([{Meter,Reading}|T],String)->
  Meter_n = extract_address(Meter),
  Meter_b = <<Meter_n:?NODE_BITS>>,
  Reading_b = <<Reading:?READING_BITS>>,
  NewString = <<Meter_b/bitstring, Reading_b/bitstring, String/bitstring>>,
  data_to_bits(T,NewString).


bitstring_to_binary(Bitstring) ->
  Size = bit_size(Bitstring),
  Stuff = 8 - (Size rem 8),
  case Stuff of
    0 -> Bitstring;
    Other -> << 0:Other , Bitstring/bitstring>>
  end.






%% todo implement in a less stupid way
extract_address(NodeNameAtom)->
  case NodeNameAtom of
    node_1 -> 1;
    node_2-> 2;
    node_3 -> 3;
    node_4 -> 4;
    node_5 -> 5;
    node_6 -> 6;
    node_7 -> 7;
    node_8 -> 8;
    node_9 -> 9;
    node_10 -> 10;
    node_11 -> 11;
    node_12 -> 12;
    node_13 -> 13;
    node_14 -> 14;
    node_15 -> 15
  end.

extract_name(Number) ->
  case Number of
    1 -> node_1;
    2 -> node_2;
    3 -> node_3;
    4 -> node_4;
    5 -> node_5;
    6 -> node_6;
    7 -> node_7;
    8 -> node_8;
    9 -> node_9;
    10 -> node_10;
    11 -> node_11;
    12 -> node_12;
    13 -> node_13;
    14 -> node_14;
    15 -> node_15
  end.


timer(Me) ->
  log:debug("INSIDE TIMER : ~w~n", [self()]) ,
  receive
    stop ->
        log:debug("STOP TIMER : ~w~n", [self()]) ,
        erlang:exit(stopped);
    restart ->
        log:debug("RESTART TIMER : ~w~n", [self()]) ,
        timer(Me)
  after ?TIMER_TIMEOUT ->
      log:debug("TIMER TIMEOUT : ~w~n", [self()]) ,
      gen_fsm:send_event(Me,timeout)
  end.


extract_nodes_from_drep(List,[]) -> List;
extract_nodes_from_drep(List,[{Node,_}|T]) ->
  extract_nodes_from_drep([Node|List],T).

 %%TODO
check_reading_and_log_time() ->
  ok.
