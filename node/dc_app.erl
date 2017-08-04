%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%       =================== under construction =====================
%%% @end
%%% Created : 01. Aug 2017 11:06
%%%-------------------------------------------------------------------
-module(dc_app).
-author("liya").

-behaviour(gen_fsm).

-include("app_macros.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  discovering/2,
handle_sync_event/3,
  handle_event/3,
  handle_info/3,
  terminate/3,
  code_change/4]).



%%-record(state, {my_pid,                % app name
%%                my_node,               % pid of protocol process in the same node
%%                my_protocol,           % pid of node process
%%                reporting_unit,        % report module
%%                reporting_file,        % file descriptor of backup report file
%%                meters,                % list of current active nodes
%%                mediums,               % list of current active nodes and their mediums
%%                exp_counter,           % current experiment iteration
%%                nrs,
%%                rd,
%%                ter,
%%                ter8,
%%                sn,
%%                timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({MyNode, MyProtocol,ReportingUnit, Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(MyNode)++"_app"),
  log:info("[~p]  ~p created ~n",[?MODULE,Me]),
  {ok,Pid}=gen_fsm:start({local,Me}, ?MODULE, {Me,MyProtocol,MyNode,ReportingUnit,Meters},[]),
  Pid.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init({Me,MyProtocol,MyNode,ReportingUnit, Meters}) ->
  Hand_shake = app_utils:hand_shake(Me,MyProtocol,1),
  case Hand_shake of
    ready ->
      log:info("[~p]  ~p initialized~n", [?MODULE,Me]),
      Fd = app_utils:open_report_file(Me),
      put(reporting_unit, ReportingUnit),
      put(reporting_file, Fd),
      app_utils:create_and_initialize_sets(Meters),
      Timerpid = erlang:spawn(app_utils, timer, [Me,?DISCOVERING_TIMEOUT]),
      State = #state{
          my_pid = Me,
          my_node = MyNode,
          my_protocol = MyProtocol,
          reporting_unit = ReportingUnit,
          reporting_file = Fd,
          meters = Meters,     %TODO maybe ?METERS
          mediums = ?MEDIUMS,
          exp_counter = 1,
          sn =0,
          timer = Timerpid},
      app_utils:report_start_of_experiment(State),
      {ok, discovering, State};

    {terminate, Reason} ->
      log:critical("[~p]  handshake with ~p failed with message:~p~n",
        [?MODULE,MyProtocol,Reason]),
      {stop,{handshake_failure,Reason}}
end.


discovering(timeout,State) when State#state.sn==0 ->
  Rd = app_utils:random_elements(State#state.meters),
  Nrs = app_utils:delete_elements(State#state.meters,Rd),
  _ = app_utils:send_dreq(State#state.my_protocol,Rd,1),
  app:utils:update_tracker_requests(Rd,0),
  log:info("[~p]  first dreq sent, Rd are ~p~n",[?MODULE,Rd]),
  NewState = State#state{
               rd = Rd,
               nrs = Nrs,
               ter = [],
               ter8 = [],
               sn =1},
  {next_state,discovering, NewState};


discovering({drep,To,Data,Seq},State) when State#state.my_node == To ->
  {V,_} = lists:last(Data),
  app_utils:report_received_drep(V,To,Seq),
  log:info("[~p]  received drep from: ~p, with Seq: ~p in state discovering ~n state data:
      Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p,  ~n",
    [?MODULE,V,Seq,State#state.nrs,State#state.rd,State#state.ter,State#state.sn]),
  Nodes = app_utils:extract_nodes_from_drep([],Data),
  log:info("[~p]  extarcting nodes from drep: ~p~n",[?MODULE,Nodes]),
  Nrs = lists:subtract(State#state.nrs,Nodes),
%%  Rd = lists:subtract(lists:delete(V,State#state.rd),Nodes),
  Rd = lists:subtract(State#state.rd,Nodes), %TODO remove above line after functionality check
%%  Ter = lists:subtract(lists:umerge([State#state.ter,[V]]),lists:delete(V,Nodes)),
  Ter = lists:umerge([lists:subtract(State#state.ter,Nodes),[V]]), %TODO remove above line after funcrionality check
  ets:insert(mr_ets, Data),
  log:debug("[~p] updating state data {nrs: ~p, rd: ~p, ter: ~p }~n",[?MODULE,Nrs,Rd,Ter]),
  NewState = State#state{
                rd = Rd,
                nrs = Nrs,
                ter = Ter},
  if Rd == [] ->
    gen_fsm:send_event(State#state.my_pid, rd_empty);
    true ->[]
   end,
    {next_state, discovering, NewState};



discovering(timeout,State)->
  log:debug("[~p]  received rd_empy event in state discovering,~n State data:
   Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n" ,
    [?MODULE,State#state.nrs,State#state.rd,State#state.ter,State#state.sn]),
  Nrs_new = new_nrs_and_delete_unresponsive(State#state.rd, State#state.nrs,
    discovering,State#state.sn),
  log:debug("[~p] Nrs_new is: ~p~n", [?MODULE,Nrs_new]),
  if Nrs_new == [] ->
    Result = app_utils:check_phase1_exp(State#state.exp_counter),
    case Result of
      finish ->  ok;
      reinitialize -> ok;
      phase2 ->
        log:info("[~p]=========FINISHED reading sems in phase1, preparing for PHASE 2 ========~n",
          [?MODULE]),
        NewState = prepare_for_phase_2(State),
        app_utils:report_next_session(NewState#state.sn, NewState#state.ter),
        {next_state, collecting, NewState}
    end;
    true ->
      log:info("[~p]  received requested replies, preparing for another iteration of Sn ~p~n",
        [?MODULE,State#state.sn]),
      NewState = prepare_for_another_iteration_of_phase_1(Nrs_new,State,new_timer),
      {next_state, discovering, NewState}
  end.









handle_event({received_message, Bit_string}, StateName, State) ->
  log:info("[~p]  received bit string: ~p in state: ~p~n", [?MODULE,Bit_string,StateName]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
  To = utils:get_node_name(To_n),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(State#state.my_pid, {dreq,To,Seq}),
      {next_state, StateName, State};
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      if Data_size rem ?ENTRY_SIZE =/= 0 ->
        log:err("[~p]  received invalid data of size ~p, dropping drep~n",[?MODULE,Data_size]),
        {next_state, StateName, State};
        true ->
         {next_state, StateName, State}
        end
  end.

handle_sync_event(get_state, StateName, StateData) ->
  log:info("[~p]  entered get_state ~n",[?MODULE]),
{StateName,StateData}.


handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


new_nrs_and_delete_unresponsive(Rd,Nrs,State,Sn)->
  Nrs1  = lists:usort(lists:umerge(Nrs,Rd)),
  delete_unresponsive_nodes(Rd, Nrs1,State,Sn).


delete_unresponsive_nodes([], Nrs,_State,_Sn) -> Nrs;
delete_unresponsive_nodes([H|T], Nrs,State,Sn)->
  [{H,Val}] = ets:lookup(tracker, H),
  if State == discovering ->
    Add = ?EXTRA_DISCOVERY_TRIES;
    true ->
      Add=0
  end,
  if Val < (?MAX_DREQ_TRIES +Add) ->
    delete_unresponsive_nodes(T,Nrs,State,Sn);
    true ->
      log:debug("[~p] WARNING ~p in unresponsive, removing from current round~n",
        [?MODULE,H]),
      app_utils:report_unresponssive_node(H,Sn),
      Nrs1=lists:delete(H,Nrs),
      delete_unresponsive_nodes(T,Nrs1,State,Sn)
  end.


prepare_for_phase_2(State)->
  app_utils:report_averages(),
  app_utils:insert_nodes_to_tracker(State#state.meters),
  State#state.timer ! stop,
  Sn = State#state.sn +1,
  Nrs = State#state.meters,
  Ter8 = State#state.ter,
  log:info("[~p]  sending dreq to: ~p with sn ~p~n", [?MODULE,Ter8,Sn]),
  app_utils:send_dreq(State#state.my_protocol, Ter8, Sn),
  app_utils:update_tracker_requests_time(Ter8,Sn),
  Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?COLLECTING_TIMEOUT]),
  NewState = State#state{
    nrs = Nrs,
    ter8 = Ter8,
    sn = Sn,
    timer = Timerpid},
  NewState.


prepare_for_another_iteration_of_phase_1(Nrs_new,State,TimerFlag)->
  Rd = app_utils:random_elements(Nrs_new),
  Nrs = app_utils:delete_elements(Nrs_new,Rd),
  log:info("[~p]  sending dreq to: ~p with sn ~p~n", [?MODULE,Rd,State#state.sn]),
  app_utils:send_dreq(State#state.my_protocol,Rd,State#state.sn),
  app_utils:update_tracker_requests_time(Rd,State#state.sn),
  case TimerFlag of
    new_timer ->
      Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?DISCOVERING_TIMEOUT]);
    old_timer ->
      Timerpid = State#state.timer,
      Timerpid ! restart
  end,
  NewState = State#state{
    nrs = Nrs,
    rd = Rd,
    timer = Timerpid},
  NewState.































