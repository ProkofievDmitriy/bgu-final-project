%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2017 10:18
%%%-------------------------------------------------------------------
-module('am_sem').
-author("liya").

-behaviour(gen_fsm).
-include("app_macros.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  counting/2,
  handle_info/3,
  terminate/3,
  code_change/4]).


%% TODO: discuss initialization with dima - he should send me my name?

%%TODO: add TO in drep format.

%% TODO add a handshake before doing any activity (gen_server:call)

%%%===================================================================
%%% API
%%%===================================================================

start_link({My_node, My_protocol,_Meters}) ->
  My_name = erlang:list_to_atom(atom_to_list(My_node)++"_app"),
  log:info("~p created ~n",[My_name]),
  timer:sleep(1500),
  {ok, PID} = gen_fsm:start_link({local, My_name}, ?MODULE,{My_name,My_protocol,My_node}, []),
  PID.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================


%% My_name - self
%% My_protocol - the gen server of the protocol in the same node
init({My_name,My_protocol,My_node}) ->
  Hand_shake =hand_shake(My_name,My_protocol,1),
  case Hand_shake of
   ready ->
    log:info("~p initialized~n", [My_name]),
     {ok, counting, {My_name,My_protocol,My_node,0,0}};
    {terminate, Reason} ->
      log:critical("handshake with ~p failed with message: ~p~n", [My_protocol,Reason]),
      {stop,{handshake_failure,Reason}}
  end.


%%TODO - trigger increment event
counting(increment, {My_name,My_protocol,My_node,Counter,Sn}) ->
  io:format("~p is icrementing, last reading: ~p~n",[My_name,Counter]),
  {next_state, counting, {My_name,My_protocol,My_node,Counter+1,Sn}};

%%TODO - consider replacing with 2 function using pattern matching on To(Me/other)
counting({dreq,To,Seq},{My_name,My_protocol,My_node,Counter,Sn}) ->
  log:debug("~p received dreq with Sn ~p~n", [My_name,Seq] ),
  %% if the dreq was meant to me
  if To == My_node ->
    %% if the dreq has a bigger sequence number - send reading and update my sn
    if Sn=<Seq ->
      %% sending reading
      log:info("~p is sending reading ~n", [My_name] ),
      _Ok = send_drep (My_protocol,[{My_name,Counter}|[]],Seq),
      %% returning to the same state with updated sequence number
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Seq}};
    %% if seq lower or equals - ignore
      true ->
        log:debug("~p received dreq ,with Seq ~p, local Sn ~p, ignoring~n", [My_name,Seq,Sn] ),
        {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
    end;
    %% if the dreq was not meant to me pass it back
    true ->
      log:err("~p received dreq with destination missmatch, passing on to ~p~n", [My_name,To] ),
      _Ok = send_dreq(My_protocol, To, Seq),
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
  end;

counting({drep,To,Data,Seq},{My_name,My_protocol,My_node,Counter,Sn}) ->
  case To of
    ?DC_NODE ->
  log:info("~p received drep with Seq ~p, state data: ~p ~n", [My_name,Seq,{My_name,My_protocol,My_node,Counter,Sn}] ),
  %% if the drep has equal or newer sequence number- append my reading
  if Sn=<Seq ->
    log:debug("~p is appending reading ~n", [My_name] ),
    _Ok = send_drep(My_protocol, [{My_node,Counter}|Data],Seq),
    %% returning to the same state with updated sequence number
    {next_state, counting, {My_name,My_protocol,My_node,Counter,Seq}};
  %% if seq lower  - ignore
    true ->
      log:err("~p received drep with Seq ~p, local Sn ~p. passing drep ~n", [My_name,Seq,Sn]),
      _Ok = send_drep(My_protocol,Data, Seq),
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
      end;
    Dest ->
      log:err("~p received drep with wrong dest address of: ~p, ignoring~n",[My_name, Dest]),
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
  end;

counting(Event,{My_name,My_protocol,My_node,Counter,Sn}) ->
  log:err("~p recaived UNEXPECTED EVENT ~p~n", [My_name,Event] ),
  {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}.



handle_info(Info, StateName, State) ->
  log:err(" ~p received UNEXPECTED MESSAGE ~p in state ~ with data ~p",[self(),Info,StateName,State]),
    {next_state, StateName, State}.

terminate(Reason, StateName, State) ->
  log:info("terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [Reason,StateName,State]),
  ok.

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
      Reply =( catch protocol_interface:hand_shake(self(),?HAND_SHAKE_TIMEOUT)),
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

send_drep(My_protocol,Data,Seq) ->
  case ?TEST_MODE of
    local ->
      My_protocol ! {drep,?DC_NODE,Data,Seq},
      ok;
    integrated ->
      log:debug("sending drep to: ~p with sequence ~p~n",[?DC_NODE,Seq]) ,
     % Reply = (catch gen_server:call(My_protocol, {drep,?DC_NODE,Data,Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),

      Bit_message = message_to_bit({drep,?DC_NODE,Data,Seq}),
      log:debug ("sending bit message: ~p~n" , [Bit_message]),
      Reply = (catch protocol_interface:send_data_reply(?DC_NODE,Bit_message)),

    %  Reply = (catch protocol_interface:send_data_reply(?DC_NODE,{drep,?DC_NODE,Data,Seq})),
      case Reply of
        {ok, sent} -> ok;
        Err -> log:critical("error in gen_server:call in send_drep : ~p~n",[Err])
      end
 end .

send_dreq(My_protocol, To, Seq) ->
  case ?TEST_MODE of
    local ->
      My_protocol! {dreq, To,Seq},
      ok;

    integrated ->
      log:debug("sending dreq to: ~p with sequence ~p~n", [To,Seq]) ,
      %Reply = (catch gen_server:call(My_protocol, {dreq, To, Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),
      Bit_message = message_to_bit({dreq, To, Seq}),
      log:debug ("sending bit message: ~p~n" , [Bit_message]),
      Reply = (catch protocol_interface:send_data_request(To,Bit_message)),
      case Reply of
        ok -> ok;
        Err -> log:critical("error in gen_server:call in send_dreq : ~p~n",[Err])
      end
  end.


message_to_bit({dreq,To,Seq}) ->
  Type_b = <<?DREQ_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b,bitstring>>;

message_to_bit({drep,To,Data,Seq})->
  Data_b = data_to_bits(Data,<<>>),
  Type_b = <<?DREQ_BIT:1>>,
  Dest = extract_address(To),
  Dest_b = <<Dest:?NODE_BITS>>,
  Seq_b = <<Seq:?SEQ_BITS>>,
  <<Type_b/bitstring, Dest_b/bitstring, Seq_b,bitstring, Data_b/bitstring>>.



%%% readings order will be reversed at the end of this -> at the receiving side will be reversed back.
data_to_bits([],String) ->String;
data_to_bits([{Meter,Reading}|T],String)->
  Meter_b = <<Meter:?NODE_BITS>>,
  Reading_b = <<Reading:?READING_BITS>>,
  NewString = <<Meter_b/bitstring, Reading_b/bitstring, String/bitstring>>,
  data_to_bits(T,NewString).

