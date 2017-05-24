%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2017 11:43
%%%-------------------------------------------------------------------
-module(dc).
-author("liya").

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

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
  code_change/4]).

-include("app_macros.hrl").

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

start_link({Me, My_server,Meters}) ->
  io:format("~p created ~n",[Me]),
  gen_fsm:start_link({local, Me}, ?MODULE, {Me, My_server,Meters}, []).

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
-spec(init(Args :: term()) ->
  {ok, StateName :: atom(), StateData :: #state{}} |
  {ok, StateName :: atom(), StateData :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).


%% Me -  self
%% My_server - the gen server of the protocol in tha same node (DC)
%% Meters - list of names of the other nodes
init({Me, My_server,Meters}) ->
  io:format("~p initialized~n", [Me]),
  Nrs = Meters,
  Rd = random_elements (Nrs),                         % 1/4+9c
  Nrs1 = delete_elements (Nrs, Rd),                   % 1/3+10
  ets:new(mr_ets,[ordered_set, named_table, public]), % create M
  _Ok = send_dreq(My_server,Rd,0),                    % 1/11
  io:format("first dreq sent, Rd are ~p~n",[Rd]),
  Timerpid = erlang:spawn(?MODULE,timer,[Me]),        % 1/12
  {ok, discovering, {Me, My_server,Meters,Nrs1,Rd,[],0,Timerpid}}.

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
-spec(discovering(Event :: term(), State :: #state{}) ->
  {next_state, NextStateName :: atom(), NextState :: #state{}} |
  {next_state, NextStateName :: atom(), NextState :: #state{},
    timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).



%% phase 1 of AMR - AM
discovering({drep,Data,_Seq},{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  io:format("received drep with data ~p~n",[Data]),
  {V,_} = lists:last(Data),                         % 1/14
  Rd1 = lists:delete(V,Rd),                         % 1/15
  Ter1 = lists:umerge([Ter,[V]]),                   % 1/16-18
  Nodes = extract_nodes_from_drep([], Data),
  Nodes1 = lists:delete(V,Nodes),
  Ter2 = lists:subtract(Ter1,Nodes1),               % 1/19
  Nrs1 = lists:subtract(Nrs, Nodes),                % 1/20
  Rd2 = lists:subtract(Rd1, Nodes),                 % 1/21
  _Ok = ets:insert(mr_ets,Data),
  if Rd2 == [] ->  gen_fsm:send_event(Me, rd_empty) end,
  {next_state, discovering, {Me,My_server,Meters,Nrs1,Rd2,Ter2,Sn,Timerpid}};


%% Rd = [] , prepare for another iteration of external loop
discovering(rd_empty,{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  Nrs1 = lists:umerge(Nrs,Rd),                    % 1/23
  if
    Nrs1 == [] ->        %% external loop terminated, go ro phase 2
      Timerpid!stop,
      Sn1=Sn+1,                                      % 2/3
      Nrs1 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_server,Meters,Nrs1,Ter8,Ter1,Sn1,Timerpid1}};
    true ->              %% otherwise prepare for next iteration
      Rd1 = random_elements (Nrs1),                  % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),            % 1/10
      _Ok = send_dreq(My_server,Rd1,Sn),             % 1/11

      Timerpid!restart,                              % 1/12
      {next_state, discovering, {Me,My_server,Meters,Nrs2,Rd1,Ter,Sn,Timerpid}}
  end;

%% timout elapsed, prepare for another iteration of external loop
discovering(timeout,{Me, My_server,Meters,Nrs,Rd,Ter,Sn,Timerpid}) ->
  io:format("timeout occured~n"),
  Nrs1 = lists:umerge(Nrs,Rd),                       % 1/23
  if
    Nrs1 == [] ->        %% external loop terminated, go ro phase 2
      Timerpid!stop,
      Sn1=Sn+1,                                      % 2/3
      Nrs1 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state, collecting,
        {Me,My_server,Meters,Nrs1,Ter8,Ter1,Sn1,Timerpid1}};
    true ->              %% otherwise prepare for next iteration
      Rd1 = random_elements (Nrs1),                   % 1/9
      Nrs2 = delete_elements (Nrs1, Rd1),             % 1/10
      _Ok = send_dreq(My_server,Rd1,Sn),              % 1/11
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),   % 1/12
      {next_state, discovering, {Me,My_server,Meters,Nrs2,Rd1,Ter,Sn,Timerpid1}}
  end.

collecting({drp,Data,_Seq},{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  {V,_} = lists:last(Data),                          % 2/10
  Ter81 = lists:delete(Ter8,V),
  Ter1 = lists:umerge([Ter,[V]]),                    % 2/11
  Nodes = extract_nodes_from_drep([], Data),
  Nodes1 = lists:delete(V,Nodes),
  Ter2 = lists:subtract(Ter1,Nodes1),                % 2/16
  Nrs1 = lists:subtract(Nrs, Nodes),                 % 2/15
  _Ok = ets:insert(mr_ets,Data),
  if Ter81 == [] ->  gen_fsm:send_event(Me, ter8_empty) end,
  {next_state, collecting, {Me,My_server,Meters, Nrs1, Ter81,Ter2,Sn,Timerpid}};

collecting(ter8_empty,{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  Ter81 = lists:umerge(Nrs ,Ter8),
  if
    Nrs == []->
      Timerpid! stop,
      Sn1 = Sn+1,
      Nrs1 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_server,Meters,Nrs1,Ter8,Ter1,Sn1,Timerpid1}};
    true ->
      _Ok = send_dreq(My_server,Ter8,Sn),           % 2/7
      Timerpid ! restart,
      {next_state, collecting, {Me,My_server,Meters, Nrs, Ter81,Ter,Sn,Timerpid}}
  end;

collecting(timeout,{Me,My_server,Meters,Nrs, Ter8, Ter, Sn, Timerpid}) ->
  Ter81 = lists:umerge(Nrs ,Ter8),
  if
    Nrs == []->
      Timerpid! stop,
      Sn1 = Sn+1,
      Nrs1 = Meters,                                 % 2/4
      Ter8 = Ter,                                    % 2/5
      Ter1 = [],                                     % 2/5
      _Ok = send_dreq(My_server,Ter8,Sn1),           % 2/7
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_server,Meters,Nrs1,Ter8,Ter1,Sn1,Timerpid1}};
    true ->
      _Ok = send_dreq(My_server,Ter8,Sn),           % 2/7
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
random_indexes(_,0,List) -> List;
random_indexes(Length,Iteration,List) ->
  Index = rand:uniform(Length),
  Bool = lists:member(Index, List) ,
  if Bool -> random_indexes(Length,Iteration,List);
    true -> random_indexes(Length,Iteration-1,[Index|List])
  end.

selected_elements(_,[],New) -> New;
selected_elements(List,[H|T],New)->
  selected_elements(List,T,[lists:nth(H,List)|New]).

random_elements(List) ->
  Length = trunc(math:sqrt(lists:flatlength(List))),
  Indexes = random_indexes(Length,Length,[]),
  selected_elements(List,Indexes,[]).

delete_elements(List,[])->List;
delete_elements(List,[H|T])->
  delete_elements(lists:delete(H,List),T).

send_dreq(_,[],_) -> ok;
send_dreq(My_server, [H|T], Seq) ->
  My_server ! {dreq, H, Seq},
  send_dreq(My_server, T, Seq).

timer(Me) ->
  receive
    stop -> erlang:exit(stopped);
    restart -> timer(Me)
  after ?TIMEOUT -> gen_fsm:send_event(Me,timeout)
  end.


extract_nodes_from_drep(List,[]) -> List;
extract_nodes_from_drep(List,[{Node,_}|T]) ->
  extract_nodes_from_drep([Node|List],T).








































