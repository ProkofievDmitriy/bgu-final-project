%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Aug 2017 09:51
%%%-------------------------------------------------------------------
-module(sem).
-author("liya").

-behaviour(gen_fsm).

-include("app_macros.hrl").

%% API
-export([start_link/1, start_from_gui/1]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

%%-record(sem_state, {
%%  my_pid,
%%  my_node,
%%  my_protocol,
%%  reporting_unit,
%%  reporting_file,
%%  count
%%}).



%%%===================================================================
%%% API
%%%===================================================================


start_link({MyNode, MyProtocol,ReportingUnit, _Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(MyNode)++"_app"),
  log:info("[~p]  ~p created ~n",[?MODULE,Me]),
  {ok,Pid}=gen_fsm:start({local,Me}, ?MODULE, {Me,MyProtocol,MyNode,ReportingUnit},[]),
  Pid.


start_from_gui(_Pid)-> ok.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init({MyName,MyProtocol,MyNode,ReportingUnit}) ->
  Hand_shake = app_utils:hand_shake(MyName,MyProtocol,1),
  case Hand_shake of
    ready ->
      log:info("[~p]  ~p initialized~n", [?MODULE,MyName]),
      Fd = app_utils:open_report_file(MyName),
      put(reporting_unit, ReportingUnit),
      put(reporting_file, Fd),
      put(my_node, MyNode),
      State = #sem_state{
        my_pid = MyName,
        my_node = MyNode,
        my_protocol = MyProtocol,
        reporting_unit = ReportingUnit,
        reporting_file = Fd,
        count = 0
      },
       {ok, idle, State};

    {terminate, Reason} ->
      log:critical("[~p]  handshake with ~p failed with message:~p~n",
        [?MODULE,MyProtocol,Reason]),
      {stop,{handshake_failure,Reason}}
  end.



handle_event({received_message, Bit_string},StateName, State) ->
  log:info("[~p]  received bit string: ~p in state: ~p~n", [?MODULE,Bit_string,idle]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
  To = utils:get_node_name(To_n),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_all_state_event(State#sem_state.my_pid, {dreq,To,Seq}),
      {next_state, StateName, State};
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      EntrySize = ?ENTRY_SIZE,
      if Data_size rem EntrySize /= 0 ->
        log:err("[~p]  received invalid(~p) data of size ~p, dropping drep~n",[?MODULE, EntrySize, Data_size]),
        {next_state, StateName, State};
        true ->
          Data = app_utils:bit_to_data(Data_b,[]),
          gen_fsm:send_all_state_event(State#sem_state.my_pid, {drep, To,Data,Seq}),
          {next_state, StateName, State}
      end
  end;

handle_event({dreq,To,Seq},StateName,State) ->
  log:info("[~p]  ~p received dreq with Sn ~p~n", [?MODULE,State#sem_state.my_pid,Seq]),
  app_utils:report_received_dreq(To, Seq),
  if To == State#sem_state.my_node ->
    log:info(" [~p]   ~p is sending reading ~n", [?MODULE,State#sem_state.my_pid]),
    _Ok = app_utils:send_drep(State#sem_state.my_protocol,
      [{State#sem_state.my_node,State#sem_state.count}|[]],Seq,normal),
    {next_state, StateName, State};

    true ->
      log:err("[~p]  ~p received dreq with destination missmatch, passing on to ~p~n",
        [?MODULE,State#sem_state.my_pid,To] ),
      _Ok = app_utils:send_dreq(State#sem_state.my_protocol, To, Seq),
      {nex_state,StateName,State}
  end;

handle_event({drep,To,Data,Seq},StateName, State) when To == ?DC_NODE ->
  log:info(" [~p]   ~p received drep with Seq ~p, state data: ~p ~n",
    [?MODULE,State#sem_state.my_pid,Seq,State] ),
  case ?AMR_MODE of
    am ->
      log:debug("[~p]  ~p is appending reading ~n", [?MODULE,State#sem_state.my_pid] ),
      _Ok = app_utils:send_drep(State#sem_state.my_protocol,
        [{State#sem_state.my_node,State#sem_state.count}|Data],Seq,relay),
      {next_state,StateName,State};
    sm ->
      log:debug("[~p]  ~p is passing reading and generating a new one ~n",
        [?MODULE,State#sem_state.my_pid] ),
      _Ok1 = app_utils:send_drep(State#sem_state.my_protocol, Data,Seq,relay),
      _Ok = app_utils:send_drep(State#sem_state.my_protocol,
      [{State#sem_state.my_node,State#sem_state.count}|[]],Seq,normal),
      {next_state,StateName,State};
    naive ->
      log:debug("[~p]  ~p is relaying drep without providing response ~n",
        [?MODULE,State#sem_state.my_pid]),
      _Ok1 = app_utils:send_drep(State#sem_state.my_protocol, Data,Seq,relay),
      {next_state,StateName,State}
  end;


handle_event(Event,StateName,State) ->
  log:err("[~p]   ~p received UNEXPECTED EVENT ~p in state ~w with data ~w",
    [?MODULE,self(),Event,StateName,State]),
  {next_state, StateName, State}.


handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(Info, StateName, State) ->
  log:err("[~p]   ~p received UNEXPECTED MESSAGE ~p in state ~p with data ~p",[?MODULE,self(),Info,StateName,State]),
  {next_state, StateName, State}.

terminate(Reason, StateName, State) ->
  log:info(" [~p] terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [?MODULE,Reason,StateName,State]),
  ok.


code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
