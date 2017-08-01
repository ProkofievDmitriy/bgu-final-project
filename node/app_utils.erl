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
-export([open_report_file/1,report_start_of_experiment/1])
-export([hand_shake/3,random_elements/1,delete_elements/2,
  create_and_initialize_sets/1,timer/2,send_dreq/3]).


open_report_file(Me)->
  Name = "exp_"++?EXP_ID++"_"++atom_to_list(Me),
  {ok,File} = file:open(filename:absname(Name)),
  File.

report_start_of_experiment(State)->
  Type = {app_info,experiment_start},
  Data = [{experiment_num, State#state.exp_counter},{new_meters_list, State#state.meters}],
  _Ok = report(State,Type,Data),
  ok.

report_sent_dreq(Dest, Sn)->
  Type


report(State,Type,Data) ->
  ReportingUnit= State#state.reporting_unit,
  ReportingUnit:report(Type,Data),
  io:format(State#state.reporting_file,"~p , ~p~n",[Type,Data]),
  log:info("[~p] sending report ~p ~p~n", [?MODULE,Type,Data]),
  ok.







hand_shake(Me,My_protocol,Times) ->
  case ?TEST_MODE of
    local ->
      My_protocol! {app_handshake, {Me,dc}},  %% format: {pid/name , type(dc/sem)}
      receive
        ok -> ready;
        Err-> case Times of
                Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                  log:err("[~p]  handshake failed with err ~p, on try number: ~p , trying again~n",[?MODULE,Err,Times]),
                  hand_shake(Me,My_protocol,Times+1);
                Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                  log:err("[~p]  handshake failed with err ~p, on try number: ~p , TERMINATING~n",[?MODULE,Err,Times]),
                  {terminate, Err}
              end
      after ?HAND_SHAKE_TIMEOUT -> case Times of
                                     Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                                       log:err("[~p]  handshake timeout on try number: ~p , trying again~n",[?MODULE,Times]),
                                       hand_shake(Me,My_protocol,Times+1);
                                     Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                                       log:err("[~p]  handshake timeout on try number: ~p , TERMINATING~n",[?MODULE,Times]),
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
              log:err("[~p]  handshake timeout on try number: ~p , trying again~n",[?MODULE,Times]),
              hand_shake(Me,My_protocol,Times+1);
            Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
              log:err("[~p]  handshake timeout on try number: ~p , TERMINATING~n",[?MODULE,Times]),
              {terminate, timeout}
          end;
        Err->
          case Times of
            Times when Times< ?HAND_SHAKE_MAX_TRIES ->
              log:err("[~p]  handshake failed with err ~p, on try number: ~p , trying again~n",[?MODULE,Err,Times]),
              hand_shake(Me,My_protocol,Times+1);
            Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
              log:err("[~p]  handshake failed with err ~p, on try number: ~p , TERMINATING~n",[?MODULE,Err,Times]),
              {terminate, Err}
          end
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
  Length = trunc(math:sqrt(lists:flatlength(List))),
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
  _Ok5 = ets:insert(stats, {avg_changes,{0,0,0}}),
  _Ok1 = ets:insert(stats, {avg_reqs,{0,0,0}}),
  _OK2 = ets:insert(stats, {avg_round_time,{ 0, 0, get_current_millis()}}),
  ok.

insert_nodes_to_tracker([]) -> ok;
insert_nodes_to_tracker([H|T])->
  _Ok = ets:insert(tracker, {H,0}),
  insert_nodes_to_tracker(T).


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
      send_dreq(My_protocol, T, Seq);

    integrated ->
      [{H,Times}] = ets:lookup(tracker, H),
      log:debug("[~p]  sending dreq to: ~p for the ~p time with sequence ~p~n", [?MODULE,H,Times,Seq]),
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

message_to_bit({dreq,To,Seq}) ->
  log:debug("[~p]: TRYING sending message:: ~p~n", [?MODULE,{dreq,To,Seq}]),
  Type_b = <<?DREQ_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  log:debug("[~p]: TRYING bitmessage message:: ~p~n", [?MODULE,<<Type_b/bitstring, Dest_b/bitstring, Seq_b/bitstring>>]),
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