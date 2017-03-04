-module(load_ng_core).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/macros.hrl").


%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2, updateUpperLevelPid/2, send/2, enable/1, disable/1, handle_incoming_message/3 ]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export
-export([active/3, active/2, idle/3]).

-record(state, {routing_set,
                rreq_handling_set,
                pending_acknowledgements_set,
                self_address,
                address_length,
                bottom_level_pid,
                upper_level_pid,
                net_traversal_time,
                reporting_unit,
                r_seq_number
                }).

-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time}).
-record(rreq_handling_set_entry, {r_seq_number, destination, originator}).
-record(pending_acknowledgement, {next_hop, originator, r_seq_number, ack_received, ack_timeout}).

-record(management_message, {originator, hop_count, r_seq_number}).
-record(load_ng_packet, {medium, type, source, destination, data}).
%% ====================================================================
%% API functions
%% ====================================================================

start(Params) ->
    Timeout = proplists:get_value(timeout, Params),
	{ok,PID} = gen_fsm:start(?MODULE, Params, [{timeout, Timeout}]),
	PID.

stop(Ref)->
	gen_fsm:send_all_state_event(Ref, stop).

updateBottomLevelPid(FsmPid, BottomLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateBottomLevelPid, BottomLevelPid}).

updateUpperLevelPid(FsmPid, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevelPid, UpperLevelPid}).

send(FsmPid, {Destination, Data})->
    %default sync event timeout 5000ms
    Result = gen_fsm:sync_send_event(FsmPid, {send_message, {Destination, Data}}, ?TIMEOUT),
    ?LOGGER:info("[~p]: Send Call Result: ~p.~n", [?MODULE, Result]),
    Result.


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

generate_RACK(Destination)->
    gen_fsm:send_event(self(), {generate_rack, Destination}).

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
    PendingAcknowledgmentsSetId = ets:new(pending_acknowledgements_set, [set, public]),


    gen_fsm:send_event_after(?REMOVE_NOT_VALID_ROUTES_TIMER, remove_not_valid_routes),

    {ok, active, #state{
        routing_set = RoutingSet_Id,
        rreq_handling_set = RREQHandlingSet_Id,
        pending_acknowledgements_set = PendingAcknowledgmentsSetId,
        net_traversal_time = NetTraversalTime,
        address_length = AddressLength,
        reporting_unit = ReportingUnit,
        self_address = SelfAddress,
%        r_seq_number = 0
        r_seq_number = 245
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
    ?LOGGER:debug("[~p]: ACTIVE - Request(send_message) in idle state, Message: {~p, ~w},  StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
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
                    Result = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{Hop#routing_set_entry.medium, Hop#routing_set_entry.next_addr}, Payload}),
                    report_data_message(StateData#state.reporting_unit, ?SEND_MESSAGE, Payload),
                    {reply, Result, active, StateData}
            end
    end.


%% ============================================================================================
%% =========================================== A-SYNC States Transitions ========================
%% ============================================================================================
active(remove_not_valid_routes, StateData)->
    ExpiredRoutes = query_expired_routes(StateData#state.routing_set),
    ?LOGGER:preciseDebug("[~p]: ACTIVE - remove_not_valid_routes Routes number =  ~p.~n", [?MODULE, length(ExpiredRoutes)]),

    lists:foreach(fun({Key, _Value}) -> ets:delete(StateData#state.routing_set, Key) end, ExpiredRoutes),

    gen_fsm:send_event_after(?REMOVE_NOT_VALID_ROUTES_TIMER, remove_not_valid_routes),
    {next_state, active, StateData};

active({generate_rreq, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RREQ for Destination ~p.~n", [?MODULE, Destination]),

    Originator = StateData#state.self_address,
    HopCount = 0,
    RREQSequenceNumber = StateData#state.r_seq_number,

    Payload = prepare_payload(StateData#state.self_address, Destination, ?RREQ, [RREQSequenceNumber, Originator, HopCount]),
    case Payload of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: ACTIVE - Generating RREQ failed prepare payload: ~p.~n", [?MODULE, ErrorMessage]),
           {next_state, active, StateData};
        _ ->
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload }),
            add_new_entry_to_rreq_handling_set(StateData#state.rreq_handling_set, {StateData#state.r_seq_number, Destination, StateData#state.self_address}),
            report_management_message(StateData#state.reporting_unit, Payload),
            NewState = StateData#state{r_seq_number = (StateData#state.r_seq_number + 1) rem 256}, % increase RREQ Sequence number
            {next_state, active, NewState}
    end;

active({generate_rrep, {Destination, RREQSequenceNumber, HopCount}}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RREP for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address, Destination, ?RREP, [RREQSequenceNumber, StateData#state.self_address, HopCount]),
    Result = query_find_next_hop(Destination, StateData#state.routing_set), % {Medium, NextHopAddress}
    case Result of
        {ok, NextHop} ->
            case Payload of
                {error, ErrorMessage} ->
                    ?LOGGER:err("[~p]: ACTIVE - Generating RREP failed prepare payload: ~w.~n", [?MODULE, ErrorMessage]);
                _ ->
                    ?LOGGER:info("[~p]: ACTIVE - Generating RREP - NextHop: ~w.~n", [?MODULE, NextHop]),
                    ?DATA_LINK:send(StateData#state.bottom_level_pid, {{NextHop#routing_set_entry.medium, NextHop#routing_set_entry.next_addr}, Payload }),
                    report_management_message(StateData#state.reporting_unit, Payload)
            end;
        Error ->
            ?LOGGER:err("[~p]: ACTIVE - FAILED Generating RREP : ~w.~n", [?MODULE, Error])
    end,
    {next_state, active, StateData};


active({generate_rerr, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RRER for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address, Destination, ?RERR, []),
    report_management_message(StateData#state.reporting_unit, Payload),
    {next_state, active, StateData};

active({generate_rack, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RACK for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address, Destination, ?RACK, []),
    report_management_message(StateData#state.reporting_unit, Payload),
    {next_state, active, StateData};




% Receive Messages Handlers
active({received_message, #load_ng_packet{type = ?DATA} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - DATA Packet : ~w .~n", [?MODULE, Packet]),
    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
        true ->
            ?TRANSPORT:handle_incoming_message(StateData#state.upper_level_pid, Packet#load_ng_packet.data);
        false ->
            forward_packet(Packet, StateData)
    end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RREQ} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREQ RECEIVED : Packet : ~w .~n", [?MODULE, Packet]),
    IsValidPacket = isValidForProcessing(?RREQ, Packet, StateData),
    if  IsValidPacket ->
        %install reverse route
        add_new_entry_to_routing_set(StateData#state.routing_set,
                                     Packet#load_ng_packet.data#management_message.originator,
                                     Packet#load_ng_packet.source,
                                     Packet#load_ng_packet.medium,
                                     Packet#load_ng_packet.data#management_message.hop_count,
                                     Packet#load_ng_packet.data#management_message.r_seq_number),
        case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
            true ->
                RREQSequenceNumber = Packet#load_ng_packet.data#management_message.r_seq_number,
                Originator = Packet#load_ng_packet.data#management_message.originator,
                HopCount = Packet#load_ng_packet.data#management_message.hop_count,
                generate_RREP({Originator, RREQSequenceNumber, HopCount});
            false -> % this router is not a destination, forwarding message
                forward_packet(Packet, StateData)
        end;
        true ->
            ?LOGGER:debug("[~p]: ACTIVE - RREQ NOT VALID, Packet DROPPED.~n", [?MODULE]),
            ok
        end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RREP} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREP RECEIVED : Packet : ~w .~n", [?MODULE, Packet]),
    IsValidPacket = isValidForProcessing(?RREP, Packet, StateData),
    if IsValidPacket ->
        %install forward route
        add_new_entry_to_routing_set(StateData#state.routing_set,
                                     Packet#load_ng_packet.data#management_message.originator,
                                     Packet#load_ng_packet.source,
                                     Packet#load_ng_packet.medium,
                                     Packet#load_ng_packet.data#management_message.hop_count,
                                     Packet#load_ng_packet.data#management_message.r_seq_number),
        case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
                true ->
                    %TODO Remove rreq handling entry
                    %TODO generate RACK
                    ok;
                false ->
                    forward_packet(Packet, StateData)
        end;
        true ->
          ?LOGGER:debug("[~p]: ACTIVE - RREP NOT VALID, Packet DROPPED.~n", [?MODULE]),
          ok
        end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RERR} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RERR : Packet : ~w .~n", [?MODULE, Packet]),
    isValidForProcessing(?RERR, Packet, StateData),

    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
                true ->
                    RREQSequenceNumber = Packet#load_ng_packet.data#management_message.r_seq_number,
                    %TODO validate RERR

                    Originator = Packet#load_ng_packet.data#management_message.originator,
                    HopCount = Packet#load_ng_packet.data#management_message.hop_count,
    %                add_new_entry_to_routing_set(StateData#state.routing_set, Originator, Packet#load_ng_packet.source, Packet#load_ng_packet.medium, HopCount, RREQSequenceNumber),
                    %TODO Update BIDIRECTIONAL LINK
                    generate_RREP({Originator, RREQSequenceNumber, HopCount});
                false ->
                    ok
            end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RACK} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RACK : Packet : ~p .~n", [?MODULE, Packet]),
    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
            true ->
                RREQSequenceNumber = Packet#load_ng_packet.data#management_message.r_seq_number,
                %TODO validate RACK

                Originator = Packet#load_ng_packet.data#management_message.originator,
                HopCount = Packet#load_ng_packet.data#management_message.hop_count,
%                add_new_entry_to_routing_set(StateData#state.routing_set, Originator, Packet#load_ng_packet.source, Packet#load_ng_packet.medium, HopCount, RREQSequenceNumber),
                %TODO Update BIDIRECTIONAL LINK
                generate_RREP({Originator, RREQSequenceNumber, HopCount});
            false ->
                ok
        end,
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
            StartTime = get_current_millis(),
            receive after 2 * State#state.net_traversal_time ->
                NextHop = query_find_next_hop(Destination, State#state.routing_set),
                case NextHop of
                    {ok, Hop} -> Hop;
                    _ ->
                        ?LOGGER:err("[~p]: get_next_hop TIMEOUT EXCEEDED : ~p.~n", [?MODULE, get_current_millis() - StartTime]),
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
    ?LOGGER:debug("[~p]: prepare_payload : MessageType: ~p, Source: ~p , Destination: ~p, DataLengthInBytes: ~p~n", [?MODULE, BinaryMessageType, BinarySource, BinaryDestination, BinaryDataLengthInBytes]),
    if (BinaryDataLengthInBytes =< ?MAX_DATA_LENGTH) ->
            Payload = <<BinaryMessageType/bitstring, BinarySource/bitstring, BinaryDestination/bitstring, BinaryDataLengthInBytes:?DATA_LENGTH_SIZE , BinaryData/bitstring>>,
            ?LOGGER:preciseDebug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: ~p bytes , with maximum allowed: ~p ~n", [?MODULE, BinaryDataLengthInBytes, ?MAX_DATA_LENGTH]),
            {error, "Binary Data Length exceeded"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Packet deserialization utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deserializePayload(Payload)->
    <<Type:?MESSAGE_TYPE_LENGTH, Source:?ADDRESS_LENGTH, Destination:?ADDRESS_LENGTH, DataLength:?DATA_LENGTH_SIZE, RestData/bitstring>> = Payload,
    DataLengthBits = DataLength * 8, % number of bytes
    <<Data:DataLengthBits/bitstring, _Rest/bitstring>> = RestData,
    Packet = #load_ng_packet{
        type = Type,
        source = Source,
        destination = Destination
    },
    ?LOGGER:preciseDebug("[~p]: deserializePayload : Packet : ~w ~n", [?MODULE, Packet]),
    deserializeMessage(Packet, Data).


deserializeMessage(#load_ng_packet{type = ?DATA} = Packet, Data)->
    Packet#load_ng_packet{data=Data};

deserializeMessage(Packet, Data)->
    ?LOGGER:preciseDebug("[~p]: deserializePayload : Packet : ~w ~n", [?MODULE, Packet]),
    RREQMessageData = binary_to_list(Data),
    RREQSequenceNumber = lists:nth(1, RREQMessageData),
    Originator = lists:nth(2, RREQMessageData),
    HopCount = lists:nth(3, RREQMessageData) + 1,
    ManagementMessage = #management_message{
        originator = Originator,
        r_seq_number = RREQSequenceNumber,
        hop_count= HopCount
    },
    Packet#load_ng_packet{data = ManagementMessage}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

amIDestination(Destination, SelfAddress)->
    case Destination of
        SelfAddress ->
            true;
        _Else ->
            false
    end.

isValidForProcessing(Type, Packet, State) when Type =:= ?RREP ->
    isValidForProcessing(?RREQ, Packet, State);

isValidForProcessing(Type, Packet, State) when Type =:= ?RREQ ->
    if Packet#load_ng_packet.data#management_message.originator =:= State#state.self_address -> false; %this router is the originator of request
       true ->
            case contains_is_rreq_handling_set(State#state.rreq_handling_set,
                                         {Packet#load_ng_packet.data#management_message.r_seq_number,
                                          Packet#load_ng_packet.destination,
                                          Packet#load_ng_packet.data#management_message.originator}) of %preventing flooding - RREQ will be not flooded , single retransmit
                false ->
                    case query_find_next_hop(Packet#load_ng_packet.data#management_message.originator, State#state.routing_set) of
                      {ok, Entry } -> %found next hop (route exists)
                        IsGreater = is_sequence_number_grater(Packet#load_ng_packet.data#management_message.r_seq_number, Entry#routing_set_entry.r_seq_number),
                        if IsGreater ->
                            true;
                            true ->
                                    ?LOGGER:info("[~p]: isValidForProcessing : Routing Table already contains entry for originator ~p.~n", [?MODULE, Entry]),
                                false
                        end;
                      Else ->
                            ?LOGGER:preciseDebug("[~p]: isValidForProcessing : Not Found single route entry: ~p.~n", [?MODULE, Else]),
                            true
                    end;
                true ->
                    ?LOGGER:info("[~p]: isValidForProcessing : RREQ ALREADY HANDLED.~n", [?MODULE]),
                    false
                end
    end.

%broadcast forwarding
forward_packet(#load_ng_packet{type = ?RREQ} = Packet, StateData) -> % only RREQ forwarded to BROADCAST_ADDRESS
    ?LOGGER:info("[~p]: forward_packet : RREQ in BROADCAST.~n", [?MODULE]),
    Payload = prepare_payload(StateData#state.self_address, Packet#load_ng_packet.destination, ?RREQ, Packet#load_ng_packet.data),
    Result = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload}),
    report_management_message(StateData#state.reporting_unit, Payload),
    Result;

%unicast forwarding
forward_packet(Packet, StateData) ->
    QueryResult = query_find_next_hop(Packet#load_ng_packet.destination, StateData#state.routing_set), % {Medium, NextHopAddress}
    case QueryResult of
        {ok, NextHop} ->
            ?LOGGER:info("[~p]: forward_packet : to ~p, in UNICAST. Packet: ~p.~n", [?MODULE, NextHop, Packet]),
            Payload = prepare_payload(StateData#state.self_address, Packet#load_ng_packet.destination, ?RREP, Packet#load_ng_packet.data),
            Result = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{NextHop#routing_set_entry.medium, NextHop#routing_set_entry.next_addr}, Payload}),
            report_management_message(StateData#state.reporting_unit, Payload),
            Result;
        Else ->
            {error, message_not_forwarded}
    end.

increment_hop_count(LoadNGPacket)->
    Data = LoadNGPacket#load_ng_packet.data,
    HopCount = Data#management_message.hop_count,
    LoadNGPacket#load_ng_packet{ data = Data#management_message{ hop_count = HopCount + 1}}.


%----------------------------------------------------------------------------
% DATA QUERIES
%----------------------------------------------------------------------------
add_new_entry_to_routing_set(RoutingSetId, Destination, NextHop, Medium, HopCount, SeqNumber)->
    Entry = #routing_set_entry{
        dest_addr = Destination,
        next_addr = NextHop,
        medium = Medium,
        hop_count = HopCount,
        r_seq_number = SeqNumber,
        bidirectional = false,
        valid_time = get_current_millis() + ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS
    },
    Result = ets:insert(RoutingSetId, {get_current_millis(), Entry}),
    ?LOGGER:info("[~p]: Routing Set updated with entry : ~w , Result : ~p .~n", [?MODULE, Entry, Result]),
    ?LOGGER:preciseDebug("[~p]: RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(RoutingSetId)]),

    Result.

query_find_next_hop(Destination, RoutingSetId)->
    Query = ets:fun2ms(fun({_Key, Entry}) when Entry#routing_set_entry.dest_addr =:= Destination -> Entry end),
    NextHop = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    Result = case NextHop of
        [Entry|[]] -> {ok, Entry};
        [_Entry|_Rest] -> {ok, get_route_set_entry_with_highest_seq_num(NextHop)};
        [] ->
            ?LOGGER:preciseDebug("[~p]: query_find_next_hop  NOT FOUND.~n", [?MODULE]),
            {?EMPTY_QUERY_RESULT, "NOT FOUND"};
        _ ->
            ?LOGGER:err("[~p]: query_find_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, NextHop]),
            {error, "UNEXPECTED RESULTS ERROR"}
    end,
    Result.

query_expired_routes(RoutingSetId)->
    CurrentMillis = get_current_millis(),
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#routing_set_entry.valid_time < CurrentMillis -> {Key, Entry} end),
    Result = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    ?LOGGER:preciseDebug("[~p]: query_expired_routes query result : ~p .~n", [?MODULE, Result]),
    Result.

add_new_entry_to_rreq_handling_set(RREQ_HandlingSet_Id, {SeqNumber, Destination, Originator})->
    Entry = #rreq_handling_set_entry{r_seq_number = SeqNumber, destination = Destination, originator = Originator},
    Result = ets:insert(RREQ_HandlingSet_Id, {get_current_millis(), Entry}),
    ?LOGGER:info("[~p]: RREQ Handling Set updated with entry : ~w, Result : ~p .~n", [?MODULE, Entry, Result]),
    ?LOGGER:preciseDebug("[~p]: RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(RREQ_HandlingSet_Id)]),
    Result.

contains_is_rreq_handling_set(RREQ_HandlingSet_Id, {SeqNumber, Destination, Originator}) ->
    Query = ets:fun2ms(fun({_Key, Entry}) when (Entry#rreq_handling_set_entry.destination =:= Destination), (Entry#rreq_handling_set_entry.originator =:= Originator) -> Entry end),
    Result = qlc:eval(ets:table(RREQ_HandlingSet_Id, [{traverse, {select, Query}}])),
    case Result of
        [Entry|[]] ->
            NewSeqNumberIsGreater = is_sequence_number_grater(SeqNumber, Entry#rreq_handling_set_entry.r_seq_number),
            if  NewSeqNumberIsGreater ->
                false; % SeqNumber > Entry#rreq_handling_set_entry.r_seq_number
                true -> true
            end;
        [] ->
            false;
        Else ->
            ?LOGGER:err("[~p]: contains_is_rreq_handling_set UNEXPECTED RESULTS: ~p.~n", [?MODULE, Else]),
            {error, "UNEXPECTED RESULTS ERROR in contains_is_rreq_handling_set"}
    end.



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
    RoutingSet =  ets:tab2list(RoutingSetId),
    report_message(?ROUTING_SET, ReportingUnit, RoutingSet).


%----------------------------------------------------------------------------
% UTILS Functions
%----------------------------------------------------------------------------
is_sequence_number_grater(S1, S2) ->
    MAXVALUE = 255,
    Result = (((S2 < S1) and (S1 - S2 =< MAXVALUE/2)) or ((S1 < S2) and (S2 - S1 > MAXVALUE/2))),
    ?LOGGER:preciseDebug("[~p]: is_sequence_number_grater  ~p > ~p ? result : ~p .~n", [?MODULE, S1, S2, Result]),
    Result.


get_current_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).


find_in_list(ComparatorFunction, [H|T])->
    lists:foldl(ComparatorFunction, H, T).

max_by_seq_num_route_set_entry_comparator(Entry1, Entry2)->
    case is_sequence_number_grater(Entry1#routing_set_entry.r_seq_number, Entry2#routing_set_entry.r_seq_number) of
        true -> Entry1;
        false -> Entry2
    end.

get_route_set_entry_with_highest_seq_num(Entries) ->
    ?LOGGER:debug("[~p]: get_route_set_entry_with_highest_seq_num Entries: ~p .~n", [?MODULE, Entries]),
    Result = find_in_list(fun max_by_seq_num_route_set_entry_comparator/2, Entries),
    ?LOGGER:temporaryInfo("[~p]: get_route_set_entry_with_highest_seq_num Result: ~p.~n", [?MODULE, Result]),
    Result.
