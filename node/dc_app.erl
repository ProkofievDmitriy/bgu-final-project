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
-export([start_link/1,configuration_updated_from_gui/2,stations_removed_from_gui/2,
  routing_tables_cleared_from_gui/1,start_from_gui/1]).

%% gen_fsm callbacks
-export([init/1,
  idle/2,
reinitialize/2,
  discovering/2,
  collecting/2,
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
%%                sn,
%%                timer,
%%                term_times}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({MyNode, MyProtocol,ReportingUnit, Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(MyNode)++"_app"),
  log:info("[~p]  ~p created ~n",[?MODULE,Me]),
  {ok,Pid}=gen_fsm:start({local,Me}, ?MODULE, {Me,MyProtocol,MyNode,ReportingUnit,Meters},[]),
  Pid.

configuration_updated_from_gui(Pid,ListOfNodesAndMediums) ->
  log:debug("[~p] configuration_updated_from_gui , Pid ~w ~n",[?MODULE,Pid]),
  gen_fsm:send_event(Pid, {configuration_update_gui, ListOfNodesAndMediums}).

stations_removed_from_gui(Pid,ListOfNodes)->
  gen_fsm:send_event(Pid, {stations_removed_gui,ListOfNodes}).

routing_tables_cleared_from_gui(Pid) ->
  gen_fsm:send_all_state_event(Pid,tables_cleared_gui).

start_from_gui(Pid)->
  gen_fsm:send_event(Pid, start_from_gui).

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
      put(my_pid, Me),
      Timerpid = erlang:spawn(app_utils, timer, [Me,?INITIALIZATION_TIMEOUT]),
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
      case ?EXP_MODE of
        hardcoded -> {ok, discovering, State};
        gui -> Timerpid!stop, {ok,idle,State}
      end;


    {terminate, Reason} ->
      log:critical("[~p]  handshake with ~p failed with message:~p~n",
        [?MODULE,MyProtocol,Reason]),
      {stop,{handshake_failure,Reason}}
end.


idle({configuration_update_gui, ListOfNodesAndMediums},State)->
  NewState = app_utils:update_mediums(ListOfNodesAndMediums,State),
  log:info("[~p]  updating configuration from ~w to ~w~n",
    [?MODULE, State#state.mediums, NewState#state.mediums]),
  {next_state, idle, NewState};

idle({stations_removed_gui,ListOfNodes},State)->
  Meters = State#state.meters,
  NewMeters = lists:subtract(Meters,ListOfNodes),
  NewState = State#state{meters = NewMeters},
  log:info("[~p]  removing station: ~w, new list: ~w~n",
    [?MODULE, ListOfNodes, NewState#state.meters]),
  {next_state, idle, NewState};

idle(start_from_gui,State)->
  Timerpid = erlang:spawn(app_utils, timer, [State#state.my_pid,?INITIALIZATION_TIMEOUT]),
  NewState = State#state{timer = Timerpid},
  log:info("[~p]  received start_from_gui mark, starting discovery ~n", [?MODULE]),
  {next_state, discovering, NewState}.




reinitialize(timeout,State) ->
  log:info("[~p]  entering reinitialization~n",[?MODULE]),
  app_utils:clear_sets(),
  Timerpid = erlang:spawn(app_utils, timer, [State#state.my_pid,?INITIALIZATION_TIMEOUT]),
  NewState = State#state{
    exp_counter = State#state.exp_counter +1,
    sn =0,
    timer = Timerpid},
  {next_state, discovering, NewState}.


discovering(timeout,State) when State#state.sn==0 ->
  app_utils:report_start_of_experiment(State),
  app_utils:create_and_initialize_sets(State#state.meters),
  Rd = app_utils:random_elements(State#state.meters),
  Nrs = app_utils:delete_elements(State#state.meters,Rd),
  _ = app_utils:send_dreq(State#state.my_protocol,Rd,1),
  app_utils:update_tracker_requests(Rd,1),
  Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?DISCOVERING_TIMEOUT]),
  log:info("[~p]  first dreq sent, Rd are ~p~n",[?MODULE,Rd]),
  NewState = State#state{
               rd = Rd,
               nrs = Nrs,
               ter = [],
               sn =1,
               timer = Timerpid},
  {next_state,discovering, NewState};



discovering(Event,State) when Event==timeout; Event==rd_empty ->
  log:debug("[~p]  received ~p event in state discovering,~n State data:
   Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p~n" ,
    [?MODULE,Event,State#state.nrs,State#state.rd,State#state.ter,State#state.sn]),
  Nrs_new = merge_and_delete_unresponsive(State#state.rd,State#state.nrs,discovering,State#state.sn),
  log:debug("[~p] Nrs_new is: ~p~n", [?MODULE,Nrs_new]),
  if Nrs_new == [] ->
    Result = check_phase1_exp(State#state.exp_counter),
    case Result of
      finish -> {stop,{shutdown,{done_experiment_number,State#state.exp_counter}},State};
      reinitialize ->
        log:info("[~p]~~~~~~~~~~~~FINISHED EXPERIMENT N. ~p, STARTING OVER ~~~~~~~~~~~~~~~~~~~~~n",
          [?MODULE, State#state.exp_counter]),
        NewState = prepare_for_reinitialization(State,Event),
        {next_state, reinitialize, NewState};
      phase2 ->
        log:info("[~p]=========FINISHED reading sems in phase1, preparing for PHASE 2 ========~n",
          [?MODULE]),
        app_utils:report_next_session(State#state.sn+1,State#state.ter),
        NewState = prepare_for_phase_2(State),
        {next_state, collecting, NewState}
    end;
    true ->
      case Event of
        rd_empty ->
          log:info("[~p]  received requested replies, preparing for another iteration of Sn ~p~n",
            [?MODULE,State#state.sn]);
        timeout ->
          log:info("[~p]  didnt receive requested replies, preparing for another iteration of Sn ~p~n",
            [?MODULE,State#state.sn])
      end,
      UpdatedState = State#state{ nrs = Nrs_new },
      NewState = prepare_for_another_iteration_of_phase_1(UpdatedState,Event),
      {next_state, discovering, NewState}
  end.



collecting(rd_empty,State) ->
  log:debug("[~p]  received rd_empy event in state collecting,~n State data:
    Nrs: ~p, Ter8: ~p, Ter: ~p, Sn: ~p, Terms_time: ~p~n" ,
    [?MODULE,State#state.nrs,State#state.rd,State#state.ter,State#state.sn,State#state.term_times]),
  Ter8 = merge_and_delete_unresponsive(State#state.rd,State#state.nrs,collecting,State#state.sn),
  log:debug("[~p] Ter8 is now: ~p~n", [?MODULE,Ter8]),
  if Ter8 == [] ->
    Result = check_phase2_exp(State#state.exp_counter, State#state.sn),
    case Result of
      finish ->  {stop,{shutdown,{done_experiment_number,State#state.exp_counter}},State};
      reinitialize ->
        log:info("[~p]~~~~~~~~~~~~FINISHED EXPERIMENT N. ~p, STARTING OVER ~~~~~~~~~~~~~~~~~~~~~n",
        [?MODULE, State#state.exp_counter]),
        NewState = prepare_for_reinitialization(State,rd_empty),
        {next_state, reinitialize, NewState};
      phase2 ->
        log:info("[~p]======== FINISHED ROUND ~p of collecting, preparing for next round =======~n",
          [?MODULE,State#state.sn]),
        app_utils:report_next_session(State#state.sn+1,State#state.ter),
        NewState = prepare_for_phase_2(State),
        {next_state, collecting, NewState}
    end;
    true ->
      log:debug("[~p]  received requested replies, preparing for another iteration of Sn ~p~n",
        [?MODULE,State#state.sn]),
      UpdatedState = State#state{ rd = Ter8 },
      NewState = prepare_for_another_iteration_of_phase_2(UpdatedState,rd_empty),
      {next_state, collecting, NewState}
  end;

collecting(timeout,State) ->
  log:debug("[~p]  received TIMEOUT event in state collecting, State data:~n
    Nrs: ~p, Ter8: ~p, Ter: ~p, Sn: ~p~n" ,
    [?MODULE,State#state.nrs,State#state.rd,State#state.ter,State#state.sn ]),
  Nrs_new = merge_and_delete_unresponsive(State#state.nrs,State#state.rd,collecting,State#state.sn),
  Ter8_new = merge_and_delete_unresponsive(State#state.rd,State#state.nrs,collecting,State#state.sn),
  if Nrs_new == [] ->
    Result = check_phase2_exp(State#state.exp_counter, State#state.sn),
    case Result of
      finish -> {stop,{shutdown,{done_experiment_number,State#state.exp_counter}},State};
      reinitialize ->
        log:info("[~p]~~~~~~~~~~~~FINISHED EXPERIMENT N. ~p, STARTING OVER ~~~~~~~~~~~~~~~~~~~~~n",
          [?MODULE, State#state.exp_counter]),
        NewState = prepare_for_reinitialization(State,timeout),
        {next_state, reinitialize, NewState};
      phase2 ->
        log:info("[~p]======== FINISHED ROUND ~p of collecting, preparing for next round =======~n",
          [?MODULE,State#state.sn]),
        app_utils:report_next_session(State#state.sn+1,State#state.ter),
        NewState = prepare_for_phase_2(State),
        {next_state, collecting, NewState}
    end;
    true ->
      log:debug("[~p]  didnt receive all requested replies, preparing for another iteration of Sn ~p~n",
        [?MODULE,State#state.sn]),
      if State#state.term_times == ?MAX_TERMINALS_TIMES ->
        log:info("[~p]  some terminals didn't respond, merging with NRS  ~n",[?MODULE]),
        UpdatedState = State#state{ rd = Ter8_new, nrs = Nrs_new},
        NewState = prepare_for_another_iteration_of_phase_2(UpdatedState,timeout),
        {next_state, collecting, NewState};
        true ->
          TermTimes = State#state.term_times + 1,
          log:info("[~p]  trying to reach terminals for the ~p time ~n",
            [?MODULE,TermTimes]),
          UpdatedState = State#state{nrs = Nrs_new,term_times = TermTimes},
          NewState = prepare_for_another_iteration_of_phase_2(UpdatedState,timeout),
          {next_state, collecting, NewState}
      end
  end.


handle_event({drep,To,Data,Seq},StateName,State) when State#state.my_node == To andalso StateName== discovering orelse State#state.my_node == To andalso StateName== collecting ->
%%handle_event({drep,To,Data,Seq},StateName,State) when StateName== discovering ;StateName== collecting ->
  {V,_} = lists:last(Data),
  app_utils:report_received_drep(V,To,Seq),
  log:info("[~p]  received drep from: ~p, with Seq: ~p in state ~p ~n state data:
      Nrs: ~p, Rd: ~p, Ter: ~p, Sn: ~p,  ~n",
    [?MODULE,V,Seq,StateName,State#state.nrs,State#state.rd,State#state.ter,State#state.sn]),
  Bool =lists:member(V,State#state.nrs) or lists:member(V,State#state.rd),
  if Bool ->
    Nodes = app_utils:extract_nodes_from_drep([],Data),
    log:info("[~p]  extarcting nodes from drep: ~p~n",[?MODULE,Nodes]),
    Nrs = lists:subtract(State#state.nrs,Nodes),
    Rd = lists:subtract(State#state.rd,Nodes),
    Ter = lists:umerge([lists:subtract(State#state.ter,Nodes),[V]]),
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
    {next_state, StateName, NewState};
    true ->
      log:info("[~p]  drep source was already read, ignoring ~n",[?MODULE]),
      {next_state, StateName, State}
  end;



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
          Data = app_utils:bit_to_data(Data_b,[]),
          gen_fsm:send_event(State#state.my_pid, {drep, To,Data,Seq}),
         {next_state, StateName, State}
        end
  end;

handle_event(tables_cleared_gui, StateName, State)->
  app_utils:report_routing_tables_cleared(State#state.exp_counter),
  {next_state, StateName, State};


handle_event(Event,StateName,State) ->
  log:err("[~p]   ~p received UNEXPECTED MESSAGE ~p in state ~w with data ~w",
    [?MODULE,self(),Event,StateName,State]),
  {next_state, StateName, State}.






handle_sync_event(get_state, StateName, StateData) ->
  log:info("[~p]  entered get_state ~n",[?MODULE]),
{StateName,StateData}.


handle_info(Info, StateName, State) ->
  log:err("[~p]   ~p received UNEXPECTED MESSAGE ~p in state ~w with data ~w",
    [?MODULE,self(),Info,StateName,State]),
  {next_state, StateName, State}.


terminate(Reason, StateName, State) ->
  State#state.timer ! stop,
  log:info("[~p]  terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [?MODULE,Reason,StateName,State]),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




merge_and_delete_unresponsive(CleaningList,ListToMerge,State,Sn)->
  ListToClean  = lists:usort(lists:umerge(CleaningList,ListToMerge)),
  delete_unresponsive_nodes(CleaningList, ListToClean,State,Sn).


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
      app_utils:report_unresponsive_node(H,Sn),
      Nrs1=lists:delete(H,Nrs),
      delete_unresponsive_nodes(T,Nrs1,State,Sn)
  end.

check_phase1_exp (CurrentExp) ->
  if ?PHASE2_COUNT==0->                      % "phase 1 only" type of experiment
    if CurrentExp == ?EXP_COUNT -> finish;   % done experimenting
      true -> reinitialize                   % more left
    end;
    true -> phase2                           % combined experiment, go to phase 2
  end.

check_phase2_exp(CurrentExp, CurrentSession)->
 if CurrentSession-1 == ?PHASE2_COUNT ->     % finished required times of phase 2 per experiment
   if CurrentExp == ?EXP_COUNT -> finish;    % done experimenting
     true ->reinitialize                     % start next experiment
   end;
   true -> phase2                            % didn't finish current experiment yet
 end.



prepare_for_phase_2(State)->
  app_utils:report_averages(),
  app_utils:insert_nodes_to_tracker(State#state.meters),
  State#state.timer ! stop,
  Sn = State#state.sn +1,
  Nrs = State#state.meters,
  Rd = State#state.ter,
  log:info("[~p]  sending dreq to: ~p with sn ~p~n", [?MODULE,Rd,Sn]),
  _ = app_utils:send_dreq(State#state.my_protocol, Rd, Sn),
  app_utils:update_tracker_requests_time(Rd,Sn),
  Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?COLLECTING_TIMEOUT]),
  NewState = State#state{
    nrs = Nrs,
    rd = Rd,
    ter = [],
    sn = Sn,
    timer = Timerpid,
    term_times = 0},
  NewState.

prepare_for_another_iteration_of_phase_1(State,TimerFlag)->
  Rd = app_utils:random_elements(State#state.nrs),
  Nrs = app_utils:delete_elements(State#state.nrs,Rd),
  log:info("[~p]  sending dreq to: ~p with sn ~p~n", [?MODULE,Rd,State#state.sn]),
  _ = app_utils:send_dreq(State#state.my_protocol,Rd,State#state.sn),
  app_utils:update_tracker_requests_time(Rd,State#state.sn),
  case TimerFlag of
    timeout ->
      Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?DISCOVERING_TIMEOUT]);
    rd_empty ->
      Timerpid = State#state.timer,
      Timerpid ! restart
  end,
  NewState = State#state{
    nrs = Nrs,
    rd = Rd,
    timer = Timerpid},
  NewState.

prepare_for_another_iteration_of_phase_2(State,TimerFlag)->
  log:info("[~p]  sending dreq to: ~p with sn ~p~n", [?MODULE,State#state.rd,State#state.sn]),
  _ = app_utils:send_dreq(State#state.my_protocol,State#state.rd,State#state.sn),
  app_utils:update_tracker_requests(State#state.rd,State#state.sn),
  case TimerFlag of
    timeout ->
      Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?COLLECTING_TIMEOUT]);
    rd_empty ->
      Timerpid = State#state.timer,
      Timerpid ! restart
  end,
  NewState = State#state{
    rd = State#state.rd,
    timer = Timerpid
  },
  NewState.


prepare_for_reinitialization(State,Event) ->
  app_utils:clear_routing_tables(State),
  case Event of rd_empty -> State#state.timer!stop  end,
  Timerpid = erlang:spawn(app_utils,timer,[State#state.my_pid,?BETWEEN_EXP_TIMEOUT]),
  NewState = State#state{timer = Timerpid},
  NewState.





























