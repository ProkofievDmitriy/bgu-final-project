-module(transport).

-behaviour(gen_fsm).
-behaviour(layer_interface).


-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/macros.hrl").

%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, send/3, send_async/4, send_async/3, updateUpperLevel/3, updateBottomLevel/3, disable/1, enable/1, handle_incoming_message/2, reset/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export.
-export([idle/3, disable/3, idle/2, disable/2]).
%% ====================================================================
%% API functionsPidToReply
%% ====================================================================


start(Params) ->
    Timeout = proplists:get_value(timeout, Params),
    {ok,PID} = gen_fsm:start(?MODULE, Params, [{timeout, Timeout}]),
	PID.

stop(FsmPid)->
	gen_fsm:send_all_state_event(FsmPid, stop).

%Controlling events
enable(FsmPid)->
    gen_fsm:send_event(FsmPid, enable).

disable(FsmPid)->
    gen_fsm:send_event(FsmPid, disable).

%Managing events
send(FsmPid, Destination, Data)->
    gen_fsm:sync_send_event(FsmPid, {send, {Destination, Data}}, ?TIMEOUT).

%Managing events
send_async(FsmPid, Destination, Data, PidToReply)->
    gen_fsm:send_event(FsmPid, {send, {Destination, Data, PidToReply}}).

send_async(FsmPid, Destination, Data)->
    gen_fsm:send_event(FsmPid, {send, {Destination, Data, undefined}}).


updateUpperLevel(FsmPid, UpperLevelModule, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevel, UpperLevelModule, UpperLevelPid}).

updateBottomLevel(FsmPid, BottomLevelModule, BottomLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateBottomLevel, BottomLevelModule, BottomLevelPid}).

handle_incoming_message(FsmPid, Message)->
    gen_fsm:send_event(FsmPid, {received_message, Message}).

reset(FsmPid) ->
    gen_fsm:sync_send_all_state_event(FsmPid, reset).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {upper_level_pid, upper_level_module, bottom_level_pid, bottom_level_module, sessions_db}).

-record(transport_header, {max, seq, id}).
-record(transport_message, {header, binary_data}).

%% ============================================================================================
%% =========================================== Init ===============================Data===========
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~w.~n", [?MODULE, Properties]),
    StartState = proplists:get_value(default_state, Properties),
    SessionsDB = ets:new(sessions_db, [set, public]),

    {ok, StartState, #state{sessions_db = SessionsDB}}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% =========================================== IDLE =========================================
idle({received_message, {Medium, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(received_message) Data: ~w ~n", [?MODULE, Data]),
    TRANSPORT_HEADER_LENGTH = ?TRANSPORT_HEADER_LENGTH,
    <<BinaryHeader:TRANSPORT_HEADER_LENGTH, BinaryData/bitstring>> = Data,
    TrasportHeader = deserialize_header(<<BinaryHeader:TRANSPORT_HEADER_LENGTH>>),
    MessageResult = get_full_message(StateData#state.sessions_db, #transport_message{header = TrasportHeader, binary_data = BinaryData}),
    case MessageResult of
        {complete, Message} ->
            ?LOGGER:info("[~p]: IDLE - Message Complete, Sending to upper level, Data: ~w~n", [?MODULE, Message]),
            UpperLevelModule = StateData#state.upper_level_module,
            UpperLevelModule:handle_incoming_message(StateData#state.upper_level_pid, {Medium, Message});
        {not_complete_message, PiecesReceived} ->
            ?LOGGER:info("[~p]: IDLE - Message NOT Complete: ~w / ~w~n", [?MODULE, PiecesReceived, TrasportHeader#transport_header.max])
    end,
    {next_state, idle, StateData};


%ASynchronous event call
idle({send, {Destination, Data, PidToReply}}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - ASYNC Event(send) , {Destination, Data} : {~p, ~w}~n", [?MODULE, Destination, Data]),
    MessagesList = get_messages_list(Data),
    BottomLevelModule = StateData#state.bottom_level_module,
    SendFunc = fun(X) -> Res = BottomLevelModule:send_async(StateData#state.bottom_level_pid, Destination, X, PidToReply),
                         ?LOGGER:info("[~p]: Sending Message: {~w, ~w}, Result: ~w~n", [?MODULE, Destination, X, Res]),
                        %  timer:sleep(50),
                         Res end,
    [ SendFunc(X) || X <- MessagesList],
    {next_state, idle, StateData};

idle(disable, StateData) ->
    %TODO truncate sessions management state
    ?LOGGER:debug("[~p]: IDLE - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
    {next_state, disable, StateData};

idle(enable, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
     {next_state, idle, StateData}.

%Synchronous event call
idle({send, {Destination, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(send) , {Destination, Data} : {~p, ~w}~n", [?MODULE, Destination, Data]),
    MessagesList = get_messages_list(Data),
    BottomLevelModule = StateData#state.bottom_level_module,
    SendFunc = fun(X) -> Res = BottomLevelModule:send(StateData#state.bottom_level_pid, Destination, X),
                         ?LOGGER:info("[~p]: Sending Message: {~w, ~w}, Result: ~w~n", [?MODULE, Destination, X, Res]),
                        %  timer:sleep(50),
                         Res end,
    ResultsList = [ SendFunc(X) || X <- MessagesList],
    Result = lists:foldl(fun(X, Acc) -> case Acc of
                                            {ok, sent} -> X;
                                            {error, _} -> Acc
                                        end
                                        end, {ok, sent}, ResultsList),
    {reply, Result, idle, StateData}.

%% =========================================== DISABLE =========================================
%% Pass all message as is - no session management enabled
disable({received_message, Message}, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(received_message) , StateData: ~w~n", [?MODULE, StateData]),
    UpperLevelModule = StateData#state.upper_level_module,
    UpperLevelModule:handle_incoming_message(StateData#state.upper_level_pid,  Message),
    {next_state, disable, StateData};

disable(disable, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
    {next_state, disable, StateData};

%Synchronous event call
disable({send, {Destination, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(send) , {Destination, Data} : {~p, ~w}, StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
    BottomLevelModule = StateData#state.bottom_level_module,
    BottomLevelModule:send(StateData#state.bottom_level_pid, Destination, Data),
    {next_state, disable, StateData};

disable(enable, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
     {next_state, idle, StateData}.

%Synchronous event call
disable({send, {Destination, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(send) , {Destination, Data} : {~p, ~w}, StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
    BottomLevelModule = StateData#state.bottom_level_module,
    Result = BottomLevelModule:send(StateData#state.bottom_level_pid, Destination, Data),
     {reply, Result, disable, StateData}.


%% ============================================================================================
%% ============================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateUpperLevel, UpperLevelModule, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevel), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid, upper_level_module=UpperLevelModule},
    ?LOGGER:debug("[~p]: updateUpperLevel, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

handle_sync_event({updateBottomLevel, BottomLevelModule, BottomLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevel), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid, bottom_level_module=BottomLevelModule},
    ?LOGGER:debug("[~p]: updateBottomLevel, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};


handle_sync_event(reset, _From, StateName, State) ->
    ?LOGGER:preciseDebug("[~p]: Handle SYNC EVENT Request(reset) ~n", [?MODULE]),
    ets:delete_all_objects(State#state.sessions_db),
    {reply, ok, StateName, State};


handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle SYNC EVENT Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Event, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData}.

%% ============================================================================================
%% ============================== INFO Event Handling =========================================
%% ============================================================================================
handle_info(Request, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Request, StateName,StateData]),
    {next_state, StateName, StateData}.
%% ============================================================================================
%% ============================ A-Sync Event Handling =========================================
%% ============================================================================================
handle_event(Event, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Event, StateName,StateData]),
    {next_state, normal, StateData}.

%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(Reason, StateName, StateData) ->
    %TODO Proper terminate with all consequences
    ?LOGGER:debug("[~p]: STUB Handle TERMINATE Request, Reason: ~p, StateName: ~p, StateData: ~w~n", [?MODULE, Reason, StateName,StateData]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    ?LOGGER:debug("[~p]: STUB Handle CODE_CHANGE Request, OldVsn: ~p, StateName: ~p, StateData: ~w, Extra: ~p.~n", [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.



%% ============================================================================================
%% ======================================== UTILS =============================================
%% ============================================================================================

generate_uuid()->
  TimeStamp = utils:get_current_millis(),
  UUID = erlang:phash2(TimeStamp),
  UUID.

get_messages_list(Data)->
    ?LOGGER:preciseDebug("[~p]: get_messages_list  Data: ~w~n", [?MODULE, Data]),
    % BinaryData = term_to_binary(Data),
    MessagesList = trim_data(Data),
    ?LOGGER:debug("[~p]: Number of messages to send : ~w~n", [?MODULE, length(MessagesList)]),
    MessagesList.

trim_data(BinaryData)->
    trim_data(BinaryData, [], 1, generate_uuid()).

trim_data(BinaryData, List, SeqNum, UUID)->
    BitLength = bit_size(BinaryData),
    ?LOGGER:debug("[~p]: MAX_DATA_LENGTH_IN_BITS = ~w , Data bits: ~w, MAX_DATA_LENGTH = ~w, List: ~w~n", [?MODULE, ?MAX_DATA_LENGTH_IN_BITS, BitLength, ?MAX_DATA_LENGTH, List]),
    if (BitLength =< ?MAX_DATA_LENGTH_IN_BITS) ->
            {_ , NewList} = lists:foldl(fun(X, {Num, Temp}) ->
                TH = #transport_header{max = SeqNum, seq = Num, id = UUID},
                BTH = serialize_header(TH),
                BPD = <<BTH/bitstring, X/bitstring>>,
                {Num + 1, [BPD |Temp]}
            end, {1, []}, List),
            ?LOGGER:preciseDebug("[~p]: trim_data  NewList: ~w~n", [?MODULE, NewList]),
            TransprotHeader = #transport_header{max = SeqNum, seq = SeqNum, id = UUID},
            BinaryTransportHeader = serialize_header(TransprotHeader),
            BinaryPacketData = <<BinaryTransportHeader/bitstring, BinaryData/bitstring>>,
            [BinaryPacketData | NewList];
        true ->
            <<Trim:?MAX_DATA_LENGTH_IN_BITS, Rest/bitstring>> = BinaryData,
            BinaryTrim = <<Trim:?MAX_DATA_LENGTH_IN_BITS>>,
            ?LOGGER:preciseDebug("[~p]: trim_data  Data: ~w, BinaryTrim: ~w, Rest: ~w~n", [?MODULE, BinaryData, BinaryTrim, Rest]),
            trim_data(Rest, List ++ [BinaryTrim], SeqNum + 1, UUID)
    end.

serialize_header(#transport_header{} = Header)->
    ?LOGGER:preciseDebug("[~p]: serialize_header  Header: ~w~n", [?MODULE, Header]),
    Max = Header#transport_header.max,
    Seq = Header#transport_header.seq,
    Id = Header#transport_header.id,
    BinaryMax = <<Max:?SESSION_MANAGEMENT_LENGTH>>,
    BinarySeq = <<Seq:?SESSION_MANAGEMENT_LENGTH>>,
    BinaryId = <<Id:?SESSION_ID_LENGHT>>,
    Result = <<BinaryMax/bitstring, BinarySeq/bitstring, BinaryId/bitstring>>,
    ?LOGGER:preciseDebug("[~p]: serialize_header  BinaryMax(~w bits): ~w, BinarySeq(~w bits): ~w, BinaryId(~w bits): ~w, Result(~w bits): ~w~n",
                [?MODULE, bit_size(BinaryMax), BinaryMax, bit_size(BinarySeq), BinarySeq,
                          bit_size(BinaryId), BinaryId, bit_size(Result), Result]),
    Result.

deserialize_header(BinaryHeader)->
    ?LOGGER:preciseDebug("[~p]: deserialize_header  BinaryHeader(~w bits): ~w~n", [?MODULE, bit_size(BinaryHeader), BinaryHeader]),
    <<Max:?SESSION_MANAGEMENT_LENGTH, Seq:?SESSION_MANAGEMENT_LENGTH, Id:?SESSION_ID_LENGHT>> = BinaryHeader,
    #transport_header{max= Max, seq = Seq, id = Id}.

get_full_message(_DB, #transport_message{ header = #transport_header{ max = 1, seq = 1}} = Message) ->
    ?LOGGER:debug("[~p]: SINGLE MESSAGE IN SESSION: ~w.~n", [?MODULE, Message]),
    {complete, Message#transport_message.binary_data};

get_full_message(DB, Message)->
    Query = ets:fun2ms(fun({Key, List}) when Key =:= Message#transport_message.header#transport_header.id -> List end),
    Result = qlc:eval(ets:table(DB, [{traverse, {select, Query}}])),
    case Result of
        [MessagesList|[]] ->
            case lists:keysearch(Message#transport_message.header#transport_header.seq, 1, MessagesList ) of
                {value, _ } ->
                    ?LOGGER:critical("[~p]: This fragment already exists!!!! : ~w.~n", [?MODULE, Message]),
                    {not_complete_message, length(MessagesList)};
                false ->
                    AllFragments = length(MessagesList) + 1 =:=  Message#transport_message.header#transport_header.max,
                    ?LOGGER:debug("[~p]: Already Received: ~w, Current Header: ~w ,AllFragments: ~w.~n", [?MODULE, length(MessagesList), Message#transport_message.header, AllFragments]),
                    if  AllFragments ->
                            Data = assemble_data(MessagesList ++ [{Message#transport_message.header#transport_header.seq, Message}]),
                            ets:delete(DB, Message#transport_message.header#transport_header.id),
                            {complete, Data};
                        true ->
                            ets:insert(DB, {Message#transport_message.header#transport_header.id, MessagesList ++ [{Message#transport_message.header#transport_header.seq, Message}]}),
                            {not_complete_message, length(MessagesList) + 1}
                    end
            end;

        [] ->
            ?LOGGER:debug("[~p]: First Message from sequence: ~w.~n", [?MODULE, Message#transport_message.header#transport_header.id]),
            ets:insert(DB, {Message#transport_message.header#transport_header.id, [{Message#transport_message.header#transport_header.seq, Message}]}),
            {not_complete_message, 1};
        Else ->
            ?LOGGER:critical("[~p]: get_full_message UNEXPECTED RESULTS: ~w.~n", [?MODULE, Else]),
            {not_complete_message, "UNEXPECTED RESULTS ERROR in get_full_message"}
    end.

assemble_data(MessagesList)->
    SortedList = lists:keysort(1, MessagesList),
    ?LOGGER:debug("[~p]: assemble_data SortedList : ~w.~n", [?MODULE, SortedList]),
    Result = lists:foldl(fun({_, Message}, Acc) ->
        BinData = Message#transport_message.binary_data,
        <<Acc/bitstring, BinData/bitstring>> end, <<>>, SortedList),
    Result.
