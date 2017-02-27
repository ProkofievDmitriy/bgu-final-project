-module(load_ng_core).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2, updateUpperLevelPid/2, send/2, enable/1, disable/1, handle_incoming_message/3 ]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export
-export([active/3, active/2, idle/3]).

-record(state, {routing_set,
                rreq_handling_set,
                self_address,
                address_length,
                bottom_level_pid,
                upper_level_pid,
                net_traversal_time,
                reporting_unit,
                rreq_seq_number
                }).

-record(load_ng_message, {medium, type, source, destination, tlv, payload}).
-record(routing_set_entry, {medium, destination, next_hop, link_cost, last_used}).
-record(rreq_handling_set_entry, {rreq_id, destination, created_time}).

-record(management_message, {originator, hop_count, rreq_seq_number}).
-record(load_ng_packet, {medium, type, source, destination, data}).
%% ====================================================================
%% API functions
%% ====================================================================

start(Params) ->
	{ok,PID} = gen_fsm:start(?MODULE, Params, []),
	PID.

stop(Ref)->
	gen_fsm:send_all_state_event(Ref, stop).

updateBottomLevelPid(FsmPid, BottomLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateBottomLevelPid, BottomLevelPid}).

updateUpperLevelPid(FsmPid, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevelPid, UpperLevelPid}).

send(FsmPid, {Destination, Data})->
    %default sync event timeout 5000ms
    gen_fsm:sync_send_event(FsmPid, {send_message, {Destination, Data}}).


enable(FsmPid)->
    gen_fsm:sync_send_event(FsmPid, enable).

disable(FsmPid)->
    gen_fsm:sync_send_event(FsmPid, disable).


handle_incoming_message(FsmPid, Medium, Payload)->
    ?LOGGER:preciseDebug("[~p]: handle_incoming_message : LoadNGPacket : ~w .~n", [?MODULE, Payload]),
    LoadNGPacket = deserializePayload(Payload),
    gen_fsm:send_event(FsmPid, {received_message, LoadNGPacket#load_ng_packet{medium = Medium}}).


%% ====================================================================
%% Internal events
%% ====================================================================
generate_RREQ(Destination)->
    gen_fsm:send_event(self(), {generate_rreq, Destination}).

generate_RREP(Destination)->
    gen_fsm:send_event(self(), {generate_rrep, Destination}).

generate_RERR(Destination)->
    gen_fsm:send_event(self(), {generate_rerr, Destination}).


%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~w.~n", [?MODULE, Properties]),
    NetTraversalTime = proplists:get_value(net_traversal_time, Properties),
    AddressLength = proplists:get_value(address_length, Properties),
    SelfAddress = proplists:get_value(?SELF_ADDRESS, Properties),
    ReportingUnit = proplists:get_value(reporting_unit, Properties),

    RoutingSet_Id = ets:new(routing_set, [set, public]), %% data entry format : {Destination , {Medium, NextHop}}
    RREQHandlingSet_Id = ets:new(rreq_handling_set, [set, public]),

    {ok, active, #state{
        routing_set = RoutingSet_Id,
        rreq_handling_set = RREQHandlingSet_Id,
        net_traversal_time = NetTraversalTime,
        address_length = AddressLength,
        reporting_unit = ReportingUnit,
        self_address = SelfAddress,
        rreq_seq_number = 0
    }}.

%% ============================================================================================
%% =========================================== SYNC States Transitions ========================
%% ============================================================================================
idle(enable, _From, StateData)->
    {reply, ok, active, StateData};

idle(Request, _From, StateData)->
    ?LOGGER:debug("[~p]: IDLE - IGNORING Request(~p),  StateData: ~w~n", [?MODULE, Request, StateData]),
    {reply, {error, "LoadNG Core NOT ACTIVE, IGNORING EVENT"}, idle, StateData}.


active(disable, _From, StateData)->
    {reply, ok, idle, StateData};


active({send_message, {Destination, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Request(send_message) in idle state, Message: {~p, ~p},  StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
    NextHop = get_next_hop(Destination, StateData), % {Medium, NextHopAddress}
    case NextHop of
        {error, ErrorMessage} ->
            {reply, {error, ErrorMessage}, active, StateData};
        Hop ->
            Payload = prepare_payload(StateData#state.self_address, Destination, ?DATA, Data), %% <<Destination/bitstring, MessageType/bitstring, Data/bitstring>>
            case Payload of
                {error, ErrorMessage} ->
                   {reply, {error, ErrorMessage}, active, StateData};
                _ ->
                    Result = ?DATA_LINK:send(StateData#state.bottom_level_pid, {Hop, Payload}),
                    report_data_message(StateData#state.reporting_unit, ?SEND_MESSAGE, Payload),
                    {reply, Result, active, StateData}
            end
    end.
%% ============================================================================================
%% =========================================== A-SYNC States Transitions ========================
%% ============================================================================================


active({generate_rreq, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RREQ for Destination ~p.~n", [?MODULE, Destination]),

    Originator = StateData#state.self_address,
    HopCount = 0,
    RREQSequenceNumber = StateData#state.rreq_seq_number,

    Payload = prepare_payload(StateData#state.self_address, Destination, ?RREQ, [RREQSequenceNumber, Originator, HopCount]),
    case Payload of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: ACTIVE - Generating RREQ failed prepare payload: ~p.~n", [?MODULE, ErrorMessage]),
            %TODO handle error on a-synch events
           {next_state, active, StateData};
        _ ->
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload }),
            update_rreq_handling_set(StateData#state.rreq_handling_set, {StateData#state.rreq_seq_number, StateData#state.self_address}),
            report_management_message(StateData#state.reporting_unit, Payload),
            NewState = StateData#state{rreq_seq_number = StateData#state.rreq_seq_number + 1}, % increase RREQ Sequence number
            {next_state, active, NewState}
    end;

active({generate_rrep, {Destination, RREQSequenceNumber, HopCount}}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RREP for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address, Destination, ?RREP, [RREQSequenceNumber, StateData#state.self_address, HopCount]),
    NextHop = get_next_hop(Destination, StateData), % {Medium, NextHopAddress}
    case Payload of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: ACTIVE - Generating RREP failed prepare payload: ~p.~n", [?MODULE, ErrorMessage]),
            %TODO handle error on a-synch events
           {next_state, active, StateData};
        _ ->
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {NextHop, Payload }),
            report_management_message(StateData#state.reporting_unit, Payload),
            {next_state, active, StateData}
    end;

active({generate_rerr, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RRER for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address, Destination, ?RERR, []),
    report_management_message(StateData#state.reporting_unit, Payload),
    {next_state, active, StateData};




% Receive Messages Handlers
active({received_message, #load_ng_packet{type = ?DATA} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - DATA Packet : ~p .~n", [?MODULE, Packet]),
    ?TRANSPORT:handle_incoming_message(StateData#state.upper_level_pid, Packet#load_ng_packet.data),
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RREQ} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREQ RECEIVED : Packet : ~p .~n", [?MODULE, Packet]),
    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
        true ->
            RREQSequenceNumber = Packet#load_ng_packet.data#management_message.rreq_seq_number,
            %TODO validate RREQ sequence number

            Originator = Packet#load_ng_packet.data#management_message.originator,
            HopCount = Packet#load_ng_packet.data#management_message.hop_count,
            update_routing_set(StateData#state.routing_set, Originator, Packet#load_ng_packet.source, Packet#load_ng_packet.medium, HopCount),
            generate_RREP({Originator, RREQSequenceNumber, HopCount});
        false ->
            ok
    end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RREP} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREP RECEIVED : Packet : ~p .~n", [?MODULE, Packet]),
    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
            true ->
                RREQSequenceNumber = Packet#load_ng_packet.data#management_message.rreq_seq_number,
                %TODO remove from handling RREQ

                Originator = Packet#load_ng_packet.data#management_message.originator,
                HopCount = Packet#load_ng_packet.data#management_message.hop_count,
                update_routing_set(StateData#state.routing_set, Originator, Packet#load_ng_packet.source, Packet#load_ng_packet.medium, HopCount);
            false ->
                NextHop = get_next_hop(Packet#load_ng_packet.destination, StateData), % {Medium, NextHopAddress}
                %TODO Handle error if no next hop found - generate RERR to source
                Payload = prepare_payload(StateData#state.self_address, Packet#load_ng_packet.destination, ?RREP, Packet#load_ng_packet.data),
                ?DATA_LINK:send(StateData#state.bottom_level_pid, {NextHop, Payload}),
                report_management_message(StateData#state.reporting_unit, Payload)
        end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RERR} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RERR : Packet : ~p .~n", [?MODULE, Packet]),
    {next_state, active, StateData}.


%% ============================================================================================
%% =========================================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateBottomLevelPid, BottomLevelPid}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid},
    ?LOGGER:debug("[~p]: updateBottomLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};


handle_sync_event({updateUpperLevelPid, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid},
    ?LOGGER:debug("[~p]: updateUpperLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};


handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle SYNC EVENT Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Event, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData}.
%% ============================================================================================
%% =========================================== INFO Handling =========================================
%% ============================================================================================

handle_info(Request, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Request, StateName,StateData]),
    {next_state, StateName, StateData}.


%% ============================================================================================
%% =========================================== A-Sync Event Handling =========================================
%% ============================================================================================
handle_event(Event, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle A-SYNC EVENT Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Event, StateName,StateData]),
    {next_state, normal, StateData}.

%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(Reason, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle TERMINATE Request, Reason: ~p, StateName: ~p, StateData: ~w~n", [?MODULE, Reason, StateName,StateData]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    ?LOGGER:debug("[~p]: STUB Handle CODE_CHANGE Request, OldVsn: ~p, StateName: ~p, StateData: ~w, Extra: ~p.~n", [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.

%% ============================================================================================
%% ======================================== Utils =========================================
%% ============================================================================================
get_next_hop(Destination, State)->
    NextHop = query_find_next_hop(Destination, State#state.routing_set),
    Result = case NextHop of
        {ok, Hop} -> Hop;
        {?EMPTY_QUERY_RESULT, _Message} ->
            ?LOGGER:debug("[~p]: get_next_hop NO HOP FOUND for destination: ~p : ~p.~n", [?MODULE, Destination, NextHop]),
            generate_RREQ(Destination),
            receive after 2 * State#state.net_traversal_time ->
                NextHop = query_find_next_hop(Destination, State#state.routing_set),
                case NextHop of
                    {ok, Hop} -> Hop;
                    _ ->
                        ?LOGGER:err("[~p]: get_next_hop TIMEOUT EXCEEDED.~n", [?MODULE]),
                        {error, "TIME OUT EXCEEDED"}
                end
            end;
        Error ->
            ?LOGGER:err("[~p]: get_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, Error]),
            Error
    end,
    ?LOGGER:debug("[~p]: get_next_hop Result: ~p.~n", [?MODULE, Result]),
    Result.


% build LoadNG Packet : <<Type:?MESSAGE_TYPE_LENGTH, Source:?ADDRESS_LENGTH, Destination:?ADDRESS_LENGTH, BinaryDataLengthInBytes:8, Data:BinaryDataLengthInBytes>>
prepare_payload(Source, Destination, MessageType, Data)->
    %TODO Remove Headers - meaningless
    BinaryDestination = <<Destination:?ADDRESS_LENGTH>>,
    BinarySource = <<Source:?ADDRESS_LENGTH>>,
    BinaryMessageType = <<MessageType:?MESSAGE_TYPE_LENGTH>>,
    BinaryData = list_to_binary(Data),
    BinaryDataLengthInBytes = byte_size(BinaryData),
    ?LOGGER:debug("[~p]: prepare_payload : MessageType: ~p, Source: ~p , Destination: ~p, DataLengthInBytes: ~p, Data: ~p.~n", [?MODULE, BinaryMessageType, BinarySource, BinaryDestination, BinaryDataLengthInBytes ,BinaryData]),
    if (BinaryDataLengthInBytes =< ?MAX_DATA_LENGTH) ->
            Payload = <<BinaryMessageType/bitstring, BinarySource/bitstring, BinaryDestination/bitstring, BinaryDataLengthInBytes:8 , BinaryData/bitstring>>,
            ?LOGGER:debug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: bit_size: ~p ~n", [?MODULE, BinaryDataLengthInBytes]),
            {error, "Binary Data Length exceeded"}
    end.

deserializePayload(Payload)->
    <<Type:?MESSAGE_TYPE_LENGTH, Source:?ADDRESS_LENGTH, Destination:?ADDRESS_LENGTH, DataLength:8, RestData/bitstring>> = Payload,
    DataLengthBits = DataLength * 8,
    <<Data:DataLengthBits/bitstring, _Rest/bitstring>> = RestData,
    Packet = #load_ng_packet{
%        medium = Medium,
        type = Type,
        source = Source,
        destination = Destination
    },
    ?LOGGER:preciseDebug("[~p]: deserializePayload : Packet : ~p ~n", [?MODULE, Packet]),
    deserializeMessage(Packet, Data).


deserializeMessage(#load_ng_packet{type = ?DATA} = Packet, Data)->
    Packet#load_ng_packet{data=Data};

deserializeMessage(#load_ng_packet{} = Packet, Data)->
    RREQMessageData = binary_to_list(Data),
    RREQSequenceNumber = lists:nth(1, RREQMessageData),
    Originator = lists:nth(2, RREQMessageData),
    HopCount = lists:nth(3, RREQMessageData) + 1,
    RREQMessage = #management_message{
        originator = Originator,
        rreq_seq_number = RREQSequenceNumber,
        hop_count= HopCount
    },
    Packet#load_ng_packet{data = RREQMessage}.



amIDestination(Destination, SelfAddress)->
    case Destination of
        SelfAddress ->
            true;
        _Else ->
            false
    end.

%----------------------------------------------------------------------------
% DATA QUERIES
%----------------------------------------------------------------------------
update_routing_set(RoutingSetId, Destination, NextHop, Medium, HopCount)->
    Result = ets:insert(RoutingSetId, {Destination, {Medium, NextHop, HopCount}}),
    ?LOGGER:info("[~p]: Routing Set updated with : {~p, {~p, ~p}} , Result : ~p .~n", [?MODULE, Destination, Medium, NextHop, Result]),
    Result.


query_find_next_hop(Destination, RoutingSetId)->
    Query = ets:fun2ms(fun({Address, NextHop}) when Address =:= Destination -> NextHop end),
    NextHop = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    Result = case NextHop of
        [{Medium, Hop, _HopCount}|[]] -> {ok, {Medium, Hop}};
        [] ->
            {?EMPTY_QUERY_RESULT, "NOT FOUND"};
        _ ->
            ?LOGGER:err("[~p]: find_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, NextHop]),
            {error, "UNEXPECTED RESULTS ERROR"}
    end,
    Result.

update_rreq_handling_set(RREQ_HandlingSet_Id, RREQ_Message_Metadata)->
    Result = ets:insert(RREQ_HandlingSet_Id, {RREQ_Message_Metadata, [] }),
    ?LOGGER:info("[~p]: RREQ Handling Set updated with RREQ_Message_Metadata : ~w, Result : ~p .~n", [?MODULE, RREQ_Message_Metadata, Result]),
    Result.


%----------------------------------------------------------------------------
% REPORT Functions
%----------------------------------------------------------------------------

%reporting process functions
report_message(Type, ReportingUnit, Message)->
    case ReportingUnit of
        undefined ->
            ?LOGGER:debug("[~p]: Reporting Unit is UNDEFINED.~n", [?MODULE]);
        _ ->
           ReportingUnit:report(Type, Message)
    end.

report_data_message(ReportingUnit, MessageType, Message)->
    Data = [{?DATA_MESSAGE_TYPE, MessageType}, {data, Message}],
    report_message(?DATA_MESSAGE, ReportingUnit, Data).


report_management_message(ReportingUnit, Message)->
    report_message(?MANAGEMENT_MESSAGE, ReportingUnit, Message).

report_routing_set(ReportingUnit, RoutingSetId)->
    RoutingSet = [],
    report_message(?ROUTING_SET, ReportingUnit, RoutingSet).

