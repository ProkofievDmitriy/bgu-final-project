%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jun 2017 12:25
%%%-------------------------------------------------------------------
-module(sm_sem).
-author("liya").

%% API
-export([]).
-compile(export_all).
-behaviour(gen_fsm).
-include("app_macros.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  counting/2
]).


%% TODO: discuss initialization with dima - he should send me my name?

%%TODO: add TO in drep format.

%% TODO add a handshake before doing any activity (gen_server:call)

%%%===================================================================
%%% API
%%%===================================================================

start_link({My_node, My_protocol,_Meters}) ->
  My_name = erlang:list_to_atom(atom_to_list(My_node)++"_app"),
  log:info(" [~p]   ~p created ~n",[?MODULE,My_name]),
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
      log:info(" [~p]   ~p initialized~n", [?MODULE,My_name]),
      {ok, counting, {My_name,My_protocol,My_node,0,0}};
    {terminate, Reason} ->
      log:critical("[~p]  handshake with ~p failed with message: ~p~n", [?MODULE,My_protocol,Reason]),
      {stop,{handshake_failure,Reason}}
  end.

counting({received_message, Bit_string},{My_name,My_protocol,My_node,Counter,Sn}) ->
  log:info(" [~p]   received bit string: ~p~n", [?MODULE,Bit_string]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
  % To_n = erlang:binary_to_integer(bitstring_to_binary(To_b)),
  log:debug("[~p]  Extracted values: Type: ~w, To_n: ~w, Seq: ~w, Data_b ~w ~n", [?MODULE,Type, To_n, Seq, Data_b ]),

  To = extract_name(To_n),
  log:debug("[~p]  Extracted name: ~p~n", [?MODULE,To]),

  %Seq = erlang:binary_to_integer(bitstring_to_binary(Seq_b)),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(My_name, {dreq,To,Seq}),
      {next_state, counting,{My_name,My_protocol,My_node,Counter,Sn}} ;
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      Entry_size = ?NODE_BITS+?READING_BITS,
      if Data_size rem Entry_size =/= 0 ->
        log:err("[~p]  received invalid data of size ~p, dropping drep~n",[?MODULE,Data_size]),
        {next_state, counting,{My_name,My_protocol,My_node,Counter,Sn}};
        true ->
          Data = bit_to_data(Data_b,[]),
          gen_fsm:send_event(My_name, {drep, To,Data,Seq}),
          {next_state, counting,{My_name,My_protocol,My_node,Counter,Sn}}
      end
  end;

%%TODO - trigger increment event
counting(increment, {My_name,My_protocol,My_node,Counter,Sn}) ->
  io:format("~p is icrementing, last reading: ~p~n",[My_name,Counter]),
  {next_state, counting, {My_name,My_protocol,My_node,Counter+1,Sn}};

%%TODO - consider replacing with 2 function using pattern matching on To(Me/other)
counting({dreq,To,Seq},{My_name,My_protocol,My_node,Counter,Sn}) ->
  log:debug("[~p]  ~p received dreq with Sn ~p~n", [?MODULE,My_name,Seq] ),
  %% if the dreq was meant to me
  if To == My_node ->
    %% if the dreq has a bigger sequence number - send reading and update my sn
    if Sn=<Seq ->
      %% sending reading
      log:info(" [~p]   ~p is sending reading ~n", [?MODULE,My_name] ),
      _Ok = send_drep (My_protocol,[{My_node,Counter}|[]],Seq),
      %% returning to the same state with updated sequence number
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Seq}};
    %% if seq lower or equals - ignore
      true ->
        log:debug("[~p]  ~p received dreq ,with Seq ~p, local Sn ~p, ignoring~n", [?MODULE,My_name,Seq,Sn] ),
        {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
    end;
  %% if the dreq was not meant to me pass it back
    true ->
      log:err("[~p]  ~p received dreq with destination missmatch, passing on to ~p~n", [?MODULE,My_name,To] ),
      _Ok = send_dreq(My_protocol, To, Seq),
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
  end;

counting({drep,To,Data,Seq},{My_name,My_protocol,My_node,Counter,Sn}) ->
  case To of
    ?DC_NODE ->
      log:info(" [~p]   ~p received drep with Seq ~p, state data: ~p ~n", [?MODULE,My_name,Seq,{My_name,My_protocol,My_node,Counter,Sn}] ),
      %% if the drep has equal or newer sequence number- append my reading
%%      if Sn=<Seq ->
        log:debug("[~p]  ~p is passing reading and generating a new one ~n", [?MODULE,My_name] ),
        _Ok1 = send_drep(My_protocol, Data,Seq), % TODO extract only the originator
        timer:sleep(?SM_WAITING_TIME),
        _Ok = send_drep(My_protocol, [{My_node,Counter}|Data],Seq),
        %% returning to the same state with updated sequence number
        {next_state, counting, {My_name,My_protocol,My_node,Counter,Seq}};
%%      %% if seq lower  - ignore
%%        true ->
%%          log:err("[~p]  ~p received drep with Seq ~p, local Sn ~p. passing drep ~n", [?MODULE,My_name,Seq,Sn]),
%%          _Ok = send_drep(My_protocol,Data, Seq),
%%          {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
%%      end;
    Dest ->
      log:err("[~p]  ~p received drep with wrong dest address of: ~p, ignoring~n",[?MODULE,My_name, Dest]),
      {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}
  end;

counting(Event,{My_name,My_protocol,My_node,Counter,Sn}) ->
  log:err("[~p]  ~p recaived UNEXPECTED EVENT ~p~n", [?MODULE,My_name,Event] ),
  {next_state, counting, {My_name,My_protocol,My_node,Counter,Sn}}.

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
      Reply =( catch protocol_interface:hand_shake(self(),?HAND_SHAKE_TIMEOUT)),
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

send_drep(My_protocol,Data,Seq) ->
  case ?TEST_MODE of
    local ->
      log:debug("[~p]  sending drep to ~p with seq ~p",[?MODULE,?DC_NODE, Seq]),
%%      Bit_message = message_to_bit({drep,?DC_NODE,Data,Seq}),
%%      My_protocol ! Bit_message,
      My_protocol! {drep,?DC_NODE,Data,Seq},
      ok;
    integrated ->
      log:debug("[~p]  sending drep to: ~p with sequence ~p with data ~p~n",[?MODULE,?DC_NODE,Seq, Data]) ,
      % Reply = (catch gen_server:call(My_protocol, {drep,?DC_NODE,Data,Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),

      Bit_message = message_to_bit({drep,?DC_NODE,Data,Seq}),
      log:debug ("[~p]: sending bit message: ~p~n" , [?MODULE,Bit_message]),
      Reply = (catch protocol_interface:send_data_reply(?DC_NODE,Bit_message)),

      %  Reply = (catch protocol_interface:send_data_reply(?DC_NODE,{drep,?DC_NODE,Data,Seq})),
      case Reply of
        {ok, sent} -> ok;
        Err -> log:critical("[~p]  error in gen_server:call in send_drep : ~p~n",[?MODULE,Err])
      end
  end .

send_dreq(My_protocol, To, Seq) ->
  case ?TEST_MODE of
    local ->
      log:debug ("sending dreq To ~p with Seq: ~p~n" , [?MODULE,To,Seq]),
%%      Bit_message = message_to_bit({dreq, To,Seq}),
%%      My_protocol! Bit_message,
      My_protocol ! {dreq, To, Seq},
      ok;

    integrated ->
      log:debug("[~p]  sending dreq to: ~p with sequence ~p~n", [?MODULE,To,Seq]) ,
      %Reply = (catch gen_server:call(My_protocol, {dreq, To, Seq}, ?PROTOCOL_REQUEST_TIMEOUT)),
      Bit_message = message_to_bit({dreq, To, Seq}),
      log:debug ("[~p]: sending bit message: ~p~n" , [?MODULE,Bit_message]),
      Reply = (catch protocol_interface:send_data_request(To,Bit_message)),
      case Reply of
        ok -> ok;
        Err -> log:critical("[~p]  error in gen_server:call in send_dreq : ~p~n",[?MODULE,Err])
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


%%bitstring_to_binary(Bitstring) ->
%%  Size = bit_size(Bitstring),
%%  Stuff = 8 - (Size rem 8),
%%  case Stuff of
%%    0 -> Bitstring;
%%    Other -> << 0:Other , Bitstring/bitstring>>
%%  end.






%% todo implement in a less stupid way
extract_address(NodeNameAtom)->
  utils:get_node_number(NodeNameAtom).

extract_name(Number) ->
  utils:get_node_name(Number).
