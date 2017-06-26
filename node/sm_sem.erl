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
        _Ok1 = send_drep(My_protocol, Data,Seq),
        sleep(?SM_WAITING_TIME),
        _Ok = send_drep(My_protocol, [{My_node,Counter}|[]],Seq),
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
