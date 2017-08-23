%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2017 13:38
%%%-------------------------------------------------------------------
-module(app_utils).
-author("liya").


-include("app_macros.hrl").
%% API

-compile(export_all).
-export([open_report_file/1,report_start_of_experiment/1,report_unresponsive_node/2,
  report_next_session/2,report_sent_dreq/2,report_sent_dreq_failed/2,report_received_drep/3]).
-export([hand_shake/3,random_elements/1,delete_elements/2,report_averages/1,
  create_and_initialize_sets/1,update_tracker_requests/2,update_tracker_requests_time/2,
  insert_requests/2,insert_time/1,update_tracker/1,timer/2,send_dreq/3,extract_nodes_from_drep/2]).


open_report_file(Me)->
  log:debug("[~p] entered open_report_file ~n", [?MODULE]),
  Name = "exp_"++?EXP_ID++"_"++atom_to_list(Me) ++ ".temp",
  {ok,File} = file:open(filename:absname(Name), write),
  File.

report_start_of_experiment(State)->
%%  log:debug("[~p] entered report_start_of_experiment , State ~p ~n", [?MODULE,State]),
  Type = {app_info,experiment_start},
  Data = [{experiment_num, State#state.exp_counter},{new_meters_list, State#state.meters}],
  _Ok = report(Type,Data),
  ok.

report_unresponsive_node(IgnoredStation,SessionNumber) ->
  Type = {app_info, ignoring_unresponsive},
  Data = [{ignored_station, IgnoredStation},{session, SessionNumber}],
  _Ok = report(Type,Data),
  ok.

report_next_session(SessionNumber, CurrentTerminals) ->
  Type = {app_info,next_session},
  Data = [{new_session,SessionNumber},{current_terminals,CurrentTerminals}],
  _Ok = report(Type,Data),
  ok.

report_removed_stations(RemovedNodes, CurrentNodes,SessionNumber)->
  Type = {app_info, removed_stations},
  Data = [{removed, RemovedNodes},{current, CurrentNodes},{session, SessionNumber}],
  _Ok = report(Type,Data),
  ok.

report_routing_tables_cleared(ExperimentNum)->
  Type = {app_info,routing_table_cleared},
  Data = [{experiment_num,ExperimentNum}],
  _Ok = report(Type,Data),
  ok.

report_sent_dreq(Destination, SessionNumber)->
  Type = {app_data_message , dreq , sent},
  Data = [{source, ?DC_NODE},{destination, Destination},{session, SessionNumber}],
  _Ok = report(Type, Data),
   ok.

report_sent_dreq_failed(Destination, SessionNumber) ->
  Type = {app_data_message , dreq , sent_failed},
  Data = [{source, ?DC_NODE},{destination, Destination},{session, SessionNumber}],
  _Ok = report(Type, Data),
  ok.

report_sent_drep_relayed(Source, SessionNumber) ->
  Type = {app_data_message, drep, relayed },
  Data = [{source, Source},{destination,?DC_NODE},{session,SessionNumber}],
  _Ok = report(Type, Data),
  ok.

report_sent_drep(Source,SessionNumber) ->
  Type = {app_data_message, drep, sent },
  Data = [{source, Source},{destination,?DC_NODE},{session,SessionNumber}],
  _Ok = report(Type, Data),
  ok.

report_sent_drep_failed(Source,SessionNumber) ->
  Type = {app_data_message, drep, sent_failed },
  Data = [{source, Source},{destination,?DC_NODE},{session,SessionNumber}],
  _Ok = report(Type, Data),
  ok.

report_received_drep(Source, Destination,SessionNumber)->
  Type = {app_data_message, drep, received},
  Data = [{source,Source},{destination,Destination},{session,SessionNumber}],
  _Ok = report(Type,Data),
  ok.

report_received_dreq(Destination, SessionNumber) ->
  Type = {app_data_message , drep , received},
  Data = [{source, ?DC_NODE}, {destination, Destination}, {session, SessionNumber}],
  _Ok = report(Type, Data),
  ok.

report(Type,Data) ->
  if ?TEST_MODE == integrated ->
    ReportingUnit = get(reporting_unit),
    log:info("[~p] sending report ~w ~w~n", [?MODULE,Type,Data]),
    ReportingUnit:report(Type,Data);
    true -> []
  end,
  Fd = get(reporting_file),
  io:format(Fd,"~w , ~w~n",[Type,Data]),
  ok.


export_db(Name)->
  if ?TEST_MODE == integrated ->
    stats_server_interface:export(Name);
    true -> []
  end.



update_mediums( [Node,Medium] ,State)->
  Mediums = State#state.mediums,
  NewMediums = lists:keyreplace(Node,1,Mediums,{Node,Medium}),
  NewState = State#state{ mediums = NewMediums},
  NewState.



hand_shake(Me,My_protocol,Times) when ?TEST_MODE == local->
  log:debug("[~p] entered local handshake , Me ~p, My_protocol ~p ,Times ~p ~n",
    [?MODULE,Me,My_protocol,Times]),
  My_protocol! {app_handshake, {Me,dc}},  %% format: {pid/name , type(dc/sem)}
  receive
    ok -> ready;
    Err->
      case Times of
        Times when Times< ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake failed with err ~p, on try number: ~p , trying again~n",
            [?MODULE,Err,Times]),
          hand_shake(Me,My_protocol,Times+1);
        Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake failed with err ~p, on try number: ~p , TERMINATING~n",
            [?MODULE,Err,Times]),
          {terminate, Err}
       end
  after ?HAND_SHAKE_TIMEOUT ->
    case Times of
      Times when Times< ?HAND_SHAKE_MAX_TRIES ->
        log:err("[~p]  handshake timeout on try number: ~p , trying again~n",[?MODULE,Times]),
        hand_shake(Me,My_protocol,Times+1);
      Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
        log:err("[~p]  handshake timeout on try number: ~p , TERMINATING~n",[?MODULE,Times]),
        {terminate, timeout}
    end
  end;

hand_shake(Me,My_protocol,Times) when ?TEST_MODE == integrated->
  log:debug("[~p] entered integrated handshake~n", [?MODULE]),
  Reply =(catch protocol_interface:hand_shake(self(), ?HAND_SHAKE_TIMEOUT))     ,
  case Reply of
    ok -> ready;
    {'EXIT',{timeout,{gen_server,call,_}}} ->
      case Times of
        Times when Times< ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake timeout on try number: ~p , trying again~n",[?MODULE,Times]),
          hand_shake(Me,My_protocol,Times+1);
        Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake timeout on try number: ~p , TERMINATING~n",[?MODULE,Times]),
          {terminate, timeout}
      end;
    Err->
      case Times of
        Times when Times< ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake failed with err ~p, on try number: ~p , trying again~n",
            [?MODULE,Err,Times]),
          hand_shake(Me,My_protocol,Times+1);
        Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
          log:err("[~p]  handshake failed with err ~p, on try number: ~p , TERMINATING~n",
            [?MODULE,Err,Times]),
          {terminate, Err}
      end
  end.

get_current_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).

my_rand(Number) ->
  RandomNumber = erlang:phash2(get_current_millis()) rem Number,
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
  if ?AMR_MODE == naive -> Length = ?NAIVE_RD_LENGTH;
    true-> Length = trunc(math:sqrt(lists:flatlength(List)))
  end,
  Indexes = random_indexes(erlang:length(List),Length,[]),
  selected_elements(List,Indexes,[]).

delete_elements(List,[])->List;
delete_elements(List,[H|T])->
  delete_elements(lists:delete(H,List),T).


create_and_initialize_sets(Meters)->
  ets:new(mr_ets,[ordered_set, named_table, public]), % create M
  ets:new(stats,[ordered_set,named_table,public]),
  ets:new(tracker, [ordered_set,named_table,public]),
  _Ok3 = insert_nodes_to_tracker(Meters),
  _Ok5 = ets:insert(stats, {avg_changes,{0,1,0}}),
  _Ok1 = ets:insert(stats, {avg_reqs,{0,1,0}}),
  _OK2 = ets:insert(stats, {avg_round_time,{ 0, 1, get_current_millis()}}),
  ok.

clear_sets() ->
  ets:delete(mr_ets),
  ets:delete(stats),
  ets:delete(tracker),
  ok.

clear_routing_tables(State) ->
  case ?TEST_MODE of
    local ->
      report_routing_tables_cleared(State#state.exp_counter);
    integrated ->
      Result = stats_server_interface:clear_routing_tables(),
      case Result of
        {ok, cleared} -> report_routing_tables_cleared(State#state.exp_counter);
        Result ->
          log:err("[~p] failed clearing routing tables, Result: ~w",
            [?MODULE,Result])
      end

    end.


insert_nodes_to_tracker([]) -> ok;
insert_nodes_to_tracker([H|T])->
  _Ok = ets:insert(tracker, {H,0}),
  insert_nodes_to_tracker(T).

remove_nodes_from_tracker([])-> ok;
remove_nodes_from_tracker([H|T])->
    _Ok = ets:delete(tracker, H),
    remove_nodes_from_tracker(T).

update_tracker_requests(Rd,Sn)->
  insert_requests( Rd, Sn),
  update_tracker(Rd).

update_tracker_requests_time(Rd,Sn)->
  insert_requests( Rd, Sn),
  insert_time(Sn),
  update_tracker(Rd).

insert_requests(Nodes, Sn) ->
  [{_,{Avg, Round,Count}}]= ets:lookup(stats,avg_reqs),
  Current = erlang:length(Nodes),
  if Round =/= Sn ->
    log:err("[~p] insert_requests Sn missmatch. popped Sn: ~p, inserted Sn: ~p ignoring insertion~n",
      [?MODULE,Round,Sn]),
    ok;

    true ->
      _Ok = ets:insert(stats, {avg_reqs, {Avg, Round, Count+Current}}),
      log:debug("[~p]  current requests info: Avg ~p, Sn ~p , Count ~p~n", [?MODULE, Avg, Round, Count+Current]),
      ok
  end.


insert_time(Sn) ->
  [{_,{Avg,Round,_Start}}] = ets:lookup(stats,avg_round_time),
  if Round =/= Sn ->
    log:err("[~p]  insert_time Sn missmatch. popped Sn: ~p, inserted Sn: ~p ignoring insertion~n",[?MODULE,Round,Sn]),
    ok;
    true ->
      _Ok = ets:insert(stats, {avg_round_time,{Avg,Round,get_current_millis()}}),
      log:debug("[~p]  current time info: Avg ~p, Sn ~p , Start ~p~n", [?MODULE,Avg, Round, get_current_millis()]),
      ok
  end.

update_tracker([]) -> ok;
update_tracker([H|T])->
  [{H,Val}] = ets:lookup(tracker, H),
  _Ok = ets:insert(tracker, {H,Val+1}),
  update_tracker(T).

report_averages(Sn) ->
  Avg_reqs = extract_avg_reqs(Sn),
  Avg_time = extract_avg_time(Sn),
  _Ok = report_to_server([{average_data_requests_per_round, Avg_reqs}, {average_time_per_round,Avg_time}]),
  ok.

report_to_server(List) ->
  log:info("[~p]  sending stats report: ~p~n",[?MODULE,List]),
  ok.

extract_avg_reqs(Sn) ->
  [{_,{Avg,Round,Count}}] = ets:lookup(stats,avg_reqs),
  New_avg = (Avg* (Round) +Count) / (Round),
  Add = if Sn == 0 -> 0; true -> 1 end,
  _Ok = ets:insert(stats, {avg_reqs,{New_avg,Round+Add,0}}),
  New_avg.

extract_avg_time(Sn) ->
  [{_,{Avg,Round,Start}}] = ets:lookup(stats,avg_round_time),
  End = get_current_millis(),
  Current = End-Start,
  New_avg = (Avg* (Round) +Current) / (Round),
  Add = if Sn == 0 -> 0; true -> 1 end,
  _Ok = ets:insert(stats, {avg_round_time,{New_avg, Round+Add,0}}),
  New_avg.


timer(Me, Time) ->
%%  log:debug("[~p]  INSIDE TIMER : ~w~n", [?MODULE,self()]) ,
  receive
    stop ->
      log:debug("[~p]  STOP TIMER : ~w~n", [?MODULE,self()]) ,
      erlang:exit(stopped);
    restart ->
      log:debug("[~p]  RESTART TIMER : ~w~n", [?MODULE,self()]) ,
      timer(Me,Time)
  after Time ->
    log:debug("[~p]  TIMER TIMEOUT : ~w~n", [?MODULE,self()]) ,
    gen_fsm:send_event(Me,timeout)
  end.

send_dreq(_,[],_) -> ok;
send_dreq(My_protocol, [H|T], Seq) ->
  case ?TEST_MODE of
    local ->
      [{H,Times}] = ets:lookup(tracker, H),
      log:debug("[~p]  sending dreq to: ~p for the ~p time with sequence ~p~n", [?MODULE,H,Times,Seq]),
%%      Bit_message = message_to_bit ({dreq,H,Seq}),
%%      My_protocol ! Bit_message,
      My_protocol ! {dreq,H,Seq},
      log:preciseDebug("sent~n"),
      _ = report_sent_dreq(H,Seq),
      send_dreq(My_protocol, T, Seq);

    integrated ->
      [{H,Times}] = ets:lookup(tracker, H),
      log:debug("[~p]  sending dreq to: ~p for the ~p time with sequence ~p~n",
        [?MODULE,H,Times,Seq]),
      %   Reply = (catch gen_server:call(My_protocol, {dreq, H, Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),
      Bit_message = message_to_bit ({dreq,H,Seq}),
      log:debug("[~p]: sending bit message:: ~p~n", [?MODULE,Bit_message]),
      Reply = protocol_interface:send_data_request(H, Bit_message),
      utils:exec_curl("132.73.198.241", "loadng", "data_req_reply", "value=3"),

      case Reply of
        {ok, sent} ->
          _ = report_sent_dreq(H, Seq),
          utils:exec_curl("132.73.198.241", "loadng", "data_req_reply", "value=0"),
          send_dreq(My_protocol, T, Seq);
        {error, timeout_exceeded} ->
          log:debug("[~p]: TIMEOUT FROM PROTOCOL ~n", [?MODULE]),
          _ = report_sent_dreq_failed(H, Seq),
          send_dreq(My_protocol, T, Seq);
        Err -> log:critical("[~p]  error in gen_server:call in send_dreq : ~p~n",[?MODULE,Err]),
          _ = report_sent_dreq_failed(H, Seq)


      end
  end.

send_drep(My_protocol,Data,Seq,RelayFlag) ->
  {Source,_} = lists:last(Data),
  case ?TEST_MODE of
    local ->
      log:debug("[~p]  sending drep to ~p with seq ~p",[?MODULE,?DC_NODE, Seq]),
%%      Bit_message = message_to_bit({drep,?DC_NODE,Data,Seq}),
%%      My_protocol ! Bit_message,
      My_protocol! {drep,?DC_NODE,Data,Seq},
      case RelayFlag of
        relay -> report_sent_drep_relayed(Source, Seq);
        normal -> report_sent_drep(Source,Seq)
      end;
    integrated ->
      log:debug("[~p]  sending drep to: ~p with sequence ~p~n",[?MODULE,?DC_NODE,Seq]) ,
      % Reply = (catch gen_server:call(My_protocol, {drep,?DC_NODE,Data,Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),

      Bit_message = message_to_bit({drep,?DC_NODE,Data,Seq}),
      log:debug ("[~p]: sending bit message: ~p~n" , [?MODULE,Bit_message]),
      Reply = (catch protocol_interface:send_data_reply(?DC_NODE,Bit_message)),
      %  Reply = (catch protocol_interface:send_data_reply(?DC_NODE,{drep,?DC_NODE,Data,Seq})),
      case Reply of
        {ok, sent} ->
          case RelayFlag of
            relay -> report_sent_drep_relayed(Source, Seq);
            normal -> report_sent_drep(Source,Seq)
          end;
        {error, timeout_exceeded} ->
          log:debug("[~p]: TIMEOUT FROM PROTOCOL ~n", [?MODULE]),
          _ = report_sent_drep_failed(Source, Seq);
        Err -> log:critical("[~p]  error in gen_server:call in send_dreq : ~p~n",[?MODULE,Err]),
          _ = report_sent_drep_failed(Source, Seq)
      end
  end .




extract_nodes_from_drep(List,[]) -> List;
extract_nodes_from_drep(List,[{Node,_}|T]) ->
  extract_nodes_from_drep([Node|List],T).





message_to_bit({dreq,To,Seq}) ->
  log:debug("[~p]: TRYING sending message:: ~p~n", [?MODULE,{dreq,To,Seq}]),
  Type_b = <<?DREQ_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  log:debug("[~p]: TRYING bitmessage message:: ~p~n",
    [?MODULE,<<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring>>]),
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring>>;

message_to_bit({drep,To,Data,Seq})->
  Data_b = data_to_bits(Data,<<>>),
  Type_b = <<?DREP_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring, Data_b/bitstring>>.

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

extract_address(NodeNameAtom)->
  utils:get_node_number(NodeNameAtom).

extract_name(Number) ->
  utils:get_node_name(Number).
