%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 16:43
%%%-------------------------------------------------------------------
-module(dc_naive).
-author("liya").

-behaviour(gen_fsm).
-compile(export_all).
%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  collecting/2,
  hand_shake/3,
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

start_link({My_node, My_protocol, Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(My_node)++"_app"),
  log:info("~p created ~n",[Me]),
  % timer:sleep(3000),
  {ok,Pid}=gen_fsm:start_link({local, Me}, ?MODULE, {Me, My_protocol,My_node,Meters}, []),
  Pid.

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
init({Me, My_protocol,My_node,Meters}) ->
  Hand_shake =hand_shake(Me,My_protocol,1),
  case Hand_shake of
    ready ->
      log:info("~p initialized~n", [Me]),
      [Rd1|Nrs] = Meters,                         % 1/4+9c
      Rd= [Rd1],
      ets:new(mr_ets,[ordered_set, named_table, public]), % create M
      ets:new(stats,[ordered_set,named_table,public]),
       _Ok = send_dreq(My_protocol,[Rd],0),                    % 1/11
       log:info("first dreq sent, Rd are ~p~n",Rd),
      _Ok1 = ets:insert(stats, {avg_reqs,{0,0, erlang:length(Rd)}}),
      log:debug("current requests info: Avg ~p, Sn ~p , Count ~p~n", [0, 0, erlang:length(Rd)]),
      _OK2 = ets:insert(stats, {avg_round_time,{ 0, 0, get_current_millis()}}),
      Timerpid = erlang:spawn(?MODULE, timer, [Me]),        % 1/12
      {ok, collecting, {Me, My_protocol,My_node,Meters,Nrs,Rd,0,Timerpid}};

    {terminate, Reason} -> log:critical("handshake with ~p failed with message:
     ~p~n", [My_protocol,Reason]),
      {stop,{handshake_failure,Reason}}
  end.
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

collecting({received_message, Bit_string},{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  log:info("received bit string: ~p~n", [Bit_string]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
  % To_n = erlang:binary_to_integer(bitstring_to_binary(To_b)),
  To = extract_name(To_n),
  %Seq = erlang:binary_to_integer(bitstring_to_binary(Seq_b)),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(Me, {dreq,To,Seq}),
      {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Rd,Sn,Timerpid}} ;
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      Entry_size = ?NODE_BITS+?READING_BITS,
      if Data_size rem Entry_size =/= 0 ->
        log:err("received invalid data of size ~p, dropping drep~n",[Data_size]),
        {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Rd,Sn,Timerpid}} ;
        true ->
          Data = bit_to_data(Data_b,[]),
          gen_fsm:send_event(Me, {drep, To,Data,Seq}),
          {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Rd,Sn,Timerpid}}
      end
  end;


collecting({drep,To,Data,Seq},{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  if To =/= My_node ->
    log:err("dc recived drep with destination address of: ~p, ignoring~n",[To]),
    {next_state, collecting, {Me,My_protocol,My_node,Meters, Nrs, Rd,Sn,Timerpid}} ;
    true ->
      {V,_} = lists:last(Data),                          % 2/10
%%      case Seq of
%%        Seq when Seq>Sn ->
%%          log:err("received drep from ~p in state collecting with higher seq of
%%      ~p,ignore.~n state data: Nrs:~p, Rd:~p,Sn:~p~n",
%%            [V,Seq,Nrs,Rd,Sn]),
%%          next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Rd,Sn,Timerpid};
%%        Seq when Seq<Sn ->
%%          % the seq num of the drep is lower than expected -> error,check
%%          log:err("received drep from ~p in state collecting with lower seq of
%%      ~p,ignore.~n state data: Nrs:~p, Rd:~p,, Sn:~p~n",
%%            [V,Seq,Nrs,Rd,Sn]),
%%          _Ok = check_reading_and_log_time(),       %%todo
%%          {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs,Rd,Sn,Timerpid}};
%%        Seq when Seq==Sn ->
          log:info("received drep from: ~p, with Seq: ~p in state collecting ~n state data: Nrs:
       ~p,Rd = ~p, Sn: ~p~n", [V,Seq,Nrs,Rd,Sn]),
          Nodes = extract_nodes_from_drep([], Data),
      log:info("extracted nodes: ~p~n",[Nodes]),
          Nrs1 = lists:subtract(Nrs, Nodes),                 % 2/15
          Rd2 = lists:subtract(Rd,Nodes),
          _Ok = ets:insert(mr_ets,Data),
          if Rd2 == [] ->  gen_fsm:send_event(Me, rd_empty),
            {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Sn,
              Timerpid}};
            true -> {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs1,Rd2,Sn,
              Timerpid}}
          end
%%      end
  end;

collecting(rd_empty,{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  log:debug("received rd_empy event in state collecting,~n State data:
    Nrs: ~p, Rd: ~p, Sn: ~p~n" , [Nrs,Rd,Sn]),
  Nrs1 =lists:usort(lists:umerge(Nrs ,Rd)),
  if Nrs1 == []->
      log:info("====== FINISHED ROUND ~p of collecting, preparing for next round =======~n",[Sn]),
    _Ok = report_averages(),
      Timerpid! stop,
      Sn1 = Sn+1,
      [Rd2|Nrs2]= Meters,     % 2/5
      Rd1=[Rd2],
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn1]),
      _Ok = send_dreq(My_protocol,Rd1,Sn1),           % 2/7
      _Ok1 = insert_requests( Rd1, Sn1),
      _Ok2 = insert_time(Sn1),
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),  % 2/8
      {next_state,collecting,
        {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Sn1,Timerpid1}};
    true ->
      log:debug("received requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      [Rd2|Nrs2] = Nrs1,
      Rd1 = [Rd2],
      log:info("sending dreq to: ~p with sn ~p~n", [Rd1,Sn]),
      _Ok = send_dreq(My_protocol,Rd1,Sn),             % 1/11
      _Ok1 = insert_requests( Rd1, Sn),
      Timerpid!restart,                              % 1/12
      {next_state, collecting, {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Sn,Timerpid}}
  end;

%% timeout elapsed, prepare for another iteration of external loop
collecting(timeout,{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  log:debug("received timeout event in state discovering, State data:~n
   Nrs: ~p, Rd: ~p, Sn: ~p~n" , [Nrs,Rd,Sn]),
  Nrs1 = lists:usort(lists:umerge(Nrs,Rd)),                       % 1/23
  if Nrs1 == [] ->        %% external loop terminated, go ro phase 2
    log:info("=========FINISHED round ~p of collecting,, preparing for next round ========~n",[Sn]),
    _Ok = report_averages(),
    Timerpid!stop,
    Sn1=Sn+1,
    [Rd2|Nrs2]= Meters,
    Rd1= [Rd2],
    _Ok = send_dreq(My_protocol,Rd1,Sn1),                    % 1/11
    log:info("sending dreq, Rd are ~p~n",[Rd]),
    _Ok1 = insert_requests( Rd, Sn1),
    _Ok2 = insert_time(Sn1),
    Timerpid1 = erlang:spawn(?MODULE, timer, [Me]),        % 1/12
    {next_state,collecting,
      {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Sn1,Timerpid1}};
    true ->              %% otherwise prepare for next iteration
      log:info("didnt receive all requested replies, preparing for another iteration of Sn ~p~n", [Sn]),
      [Rd2|Nrs2] =Nrs1,
      Rd1= [Rd2],
      log:info("sending dreq to: ~p with sn ~p~n", [Rd,Sn]),
      _Ok = send_dreq(My_protocol,Rd1,Sn),              % 1/11
      _Ok1 = insert_requests( Rd1, Sn),
      Timerpid1 = erlang:spawn(?MODULE,timer,[Me]),   % 1/12
      {next_state,collecting,
        {Me,My_protocol,My_node,Meters,Nrs2,Rd1,Sn,Timerpid1}}
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
terminate(Reason, StateName, {Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}) ->
  Timerpid! stop,
  log:info("terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [Reason,StateName,{Me,My_protocol,My_node,Meters,Nrs, Rd, Sn, Timerpid}]),
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
  log:debug("entering send_dreq ~p ~p~n", [H,Seq]),

  case ?TEST_MODE of
    local ->
      log:debug("sending dreq to: ~p with sequence ~p~n", [H,Seq]),
%%      Bit_message = message_to_bit ({dreq,H,Seq}),
%%      My_protocol ! Bit_message,
        My_protocol! {dreq,H,Seq},
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
%%  log:debug("INSIDE TIMER : ~w~n", [self()]) ,
  receive
    stop ->
%%      log:debug("STOP TIMER : ~w~n", [self()]) ,
      erlang:exit(stopped);
    restart ->
%%      log:debug("RESTART TIMER : ~w~n", [self()]) ,
      timer(Me)
  after ?TIMER_TIMEOUT ->
%%    log:debug("TIMER TIMEOUT : ~w~n", [self()]) ,
    gen_fsm:send_event(Me,timeout)
  end.


extract_nodes_from_drep(List,[]) -> List;
extract_nodes_from_drep(List,[{Node,_}|T]) ->
  extract_nodes_from_drep([Node|List],T).

report_averages() ->
  Avg_reqs = extract_avg_reqs(),
  Avg_time = extract_avg_time(),
  _Ok = report_to_server([{average_data_requests_per_round, Avg_reqs}, {average_time_per_round,Avg_time}]),
  ok.

extract_avg_reqs() ->
  [{_,{Avg,Round,Count}}] = ets:lookup(stats,avg_reqs),
  New_avg = (Avg* (Round) +Count) / (Round+1),
  _Ok = ets:insert(stats, {avg_reqs,{New_avg,Round+1,0}}),
  New_avg.

extract_avg_time() ->
  [{_,{Avg,Round,Start}}] = ets:lookup(stats,avg_round_time),
  End = get_current_millis(),
  Current = End-Start,
  New_avg = (Avg* (Round) +Current) / (Round+1),
  _Ok = ets:insert(stats, {avg_round_time,{New_avg, Round+1,0}}),
  New_avg.

insert_requests(Nodes, Sn) ->
  [{_,{Avg, Round,Count}}]= ets:lookup(stats,avg_reqs),
  Current = erlang:length(Nodes),
  if Round =/= Sn ->
    log:error("insert_requests Sn missmatch. popped Sn: ~p, inserted Sn: ~p ignoring         insertion",[Round,Sn]),
    ok;

    true ->
      _Ok = ets:insert(stats, {avg_reqs, {Avg, Round, Count+Current}}),
      log:debug("current requests info: Avg ~p, Sn ~p , Count ~p~n", [Avg, Round, Count+Current]),
      ok
  end.


insert_time(Sn) ->
  [{_,{Avg,Round,_Start}}] = ets:lookup(stats,avg_round_time),
  if Round =/= Sn ->
    log:err("insert_time Sn missmatch. popped Sn: ~p, inserted Sn: ~p ignoring         insertion",[Round,Sn]),
    ok;
    true ->
      _Ok = ets:insert(stats, {avg_round_time,{Avg,Round,get_current_millis()}}),
      log:debug("current time info: Avg ~p, Sn ~p , Start ~p~n", [Avg, Round, get_current_millis()]),
      ok
  end.



report_to_server(List) ->
  log:info("sending stats report: ~p~n",[List]),
  ok.

