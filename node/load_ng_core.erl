-module(load_ng_core).
-behaviour(gen_fsm).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/macros.hrl").


%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2, updateUpperLevelPid/2, send/2, enable/1, disable/1, handle_incoming_message/3, get_status/1 ]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export
-export([active/3, active/2, idle/3, idle/2]).

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

-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time, valid}).
-record(rreq_handling_set_entry, {r_seq_number, destination, originator, valid_time}).
-record(pending_acknowledgement_entry, {next_hop, originator, r_seq_number, ack_received, ack_timeout}).


-record(load_ng_packet, {medium, type, source, destination, originator, data}).

-record(rreq_message, {originator, destination, hop_count, r_seq_number}).
-record(rrep_message, {originator, destination, ack_required, hop_count, r_seq_number}).
-record(rack_message, {originator, destination, hop_count, r_seq_number}).
-record(rerr_message, {originator, destination, unreachable_address, r_seq_number, error_code}).


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
    ?LOGGER:dev("[~p]: handle_incoming_message : Medium: ~p, LoadNGPacket : ~w .~n", [?MODULE, ?GET_MEDIUM_NAME(Medium), Payload]),
    LoadNGPacket = deserializePayload(Payload),
    gen_fsm:send_event(FsmPid, {received_message, LoadNGPacket#load_ng_packet{medium = Medium}}).

get_status(FsmPid) ->
    gen_fsm:sync_send_all_state_event(FsmPid, get_status).


%% ====================================================================
%% Internal events
%% ====================================================================
generate_RREQ(Destination)->
    gen_fsm:send_event(self(), {generate_rreq, Destination}).

generate_RREP(MetaData)->
    gen_fsm:send_event(self(), {generate_rrep, MetaData}).

generate_RERR(MetaData)->
    gen_fsm:send_event(self(), {generate_rerr, MetaData}).

generate_RACK(MetaData)->
    gen_fsm:send_event(self(), {generate_rack, MetaData}).


%% ====================================================================
%% Init
%% ====================================================================
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
    gen_fsm:send_event_after(?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS, update_expired_routes),
    gen_fsm:send_event_after(?NET_TRAVERSAL_TIME, remove_expired_rreq),

    State = #state{
        routing_set = RoutingSet_Id,
        rreq_handling_set = RREQHandlingSet_Id,
        pending_acknowledgements_set = PendingAcknowledgmentsSetId,
        net_traversal_time = NetTraversalTime,
        address_length = AddressLength,
        reporting_unit = ReportingUnit,
        self_address = SelfAddress,
        r_seq_number = 0
    },

    {ok, active, State}.

%% ============================================================================================
%% =========================================== SYNC States Transitions ========================
%% ============================================================================================
idle(enable, _From, StateData)->
    {reply, ok, active, StateData};

idle(Request, _From, StateData)->
    ?LOGGER:debug("[~p]: IDLE - IGNORING SYNC EVENT(~p),  StateData: ~w~n", [?MODULE, Request, StateData]),
    {reply, {error, "LoadNG Core NOT ACTIVE, IGNORING EVENT"}, idle, StateData}.


active(disable, _From, StateData)->
    {reply, ok, idle, StateData};

active({send_message, {Destination, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Request(send_message) Destination: ~p, Data: ~p ~n", [?MODULE, Destination, Data]),
    NextHop = get_next_hop(Destination, StateData), % {Medium, NextHopAddress}
    case NextHop of
        {error, ErrorMessage} ->
            {reply, {error, ErrorMessage}, active, StateData};
        Hop ->
            Payload = prepare_payload(StateData#state.self_address,
                                      StateData#state.self_address,
                                      Destination,
                                      ?DATA,
                                      Data), %% <<Destination/bitstring, MessageType/bitstring, Data/bitstring>>
            case Payload of
                {error, ErrorMessage} ->
                   {reply, {error, ErrorMessage}, active, StateData};
                _ ->
                    Result = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{Hop#routing_set_entry.medium, Hop#routing_set_entry.next_addr}, Payload}),
                    report_data_message_sent(Payload, StateData),
                    {reply, Result, active, StateData}
            end;
        Else ->
            ?LOGGER:critical("[~p]: ACTIVE -Request(send_message)  - Enexpected error: ~p .~n", [?MODULE, Else]),
            {reply, Else, active, StateData}
    end.


%% ============================================================================================
%% =========================================== A-SYNC States Transitions ========================
%% ============================================================================================
idle(Request, StateData)->
    ?LOGGER:debug("[~p]: IDLE - IGNORING A-SYNC EVENT(~p),  StateData: ~w~n", [?MODULE, Request, StateData]),
    {next_state, idle, StateData}.


active(remove_not_valid_routes, StateData)->
    NotValidRoutes = query_not_valid_routes(StateData#state.routing_set),
    gen_fsm:send_event_after(?REMOVE_NOT_VALID_ROUTES_TIMER, remove_not_valid_routes),
    case NotValidRoutes of
        [] -> {next_state, active, StateData};
        _ ->
            ?LOGGER:preciseDebug("[~p]: ACTIVE - remove_not_valid_routes Routes number =  ~p.~n", [?MODULE, length(NotValidRoutes)]),
            lists:foreach(fun({Key, _Value}) -> ets:delete(StateData#state.routing_set, Key) end, NotValidRoutes),
            {next_state, active, StateData}
    end;

active(update_expired_routes, StateData)->
    ExpiredRoutes = query_expired_routes(StateData#state.routing_set),
    gen_fsm:send_event_after(?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS, update_expired_routes),
    case ExpiredRoutes of
        [] -> {next_state, active, StateData};
        _ ->
            ?LOGGER:preciseDebug("[~p]: ACTIVE - update_expired_routes Routes number =  ~p.~n", [?MODULE, length(ExpiredRoutes)]),
            lists:foreach(fun({Key, Value}) -> ets:insert(StateData#state.routing_set, {Key, Value#routing_set_entry{valid = false}}) end, ExpiredRoutes),
            {next_state, active, StateData}
    end;

active(remove_expired_rreq, StateData)->
    ExpiredRREQ = query_expired_rreq(StateData#state.rreq_handling_set),
    gen_fsm:send_event_after(?NET_TRAVERSAL_TIME, remove_expired_rreq),
    case ExpiredRREQ of
        [] -> {next_state, active, StateData};
        _ ->
            ?LOGGER:preciseDebug("[~p]: ACTIVE - remove_expired_rreq Routes number =  ~p.~n", [?MODULE, length(ExpiredRREQ)]),
            lists:foreach(fun({Key, _Value}) -> ets:delete(StateData#state.rreq_handling_set, Key) end, ExpiredRREQ),
            {next_state, active, StateData}
    end;

active({generate_rreq, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - GENERATING RREQ for Destination ~p.~n", [?MODULE, Destination]),

    Originator = StateData#state.self_address,
    HopCount = 0,
    RREQSequenceNumber = StateData#state.r_seq_number,

    Payload = prepare_payload(StateData#state.self_address,
                              StateData#state.self_address,
                              Destination,
                              ?RREQ,
                              [RREQSequenceNumber, Originator, HopCount]),
    case Payload of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: ACTIVE - GENERATING RREQ failed prepare payload: ~p.~n", [?MODULE, ErrorMessage]),
            {next_state, active, StateData};
        _ ->
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload }),
            add_new_entry_to_rreq_handling_set(StateData#state.rreq_handling_set,
                                              {StateData#state.r_seq_number, Destination, StateData#state.self_address}),
            report_management_message(Payload, StateData),
            NewState = StateData#state{r_seq_number = (StateData#state.r_seq_number + 1) rem ?SEQUENCE_NUMBER_MAX_VALUE}, % increase RREQ Sequence number
            {next_state, active, NewState}
    end;

active({generate_rrep, {Destination, RREQSequenceNumber, HopCount}}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - GENERATING RREP for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address,
                              StateData#state.self_address,
                              Destination,
                              ?RREP,
                              [RREQSequenceNumber, StateData#state.self_address, HopCount, ?ACK_REQUIRED]),

    Result = query_find_next_hop(Destination, StateData#state.routing_set), % {Medium, NextHopAddress}
    case Result of
        {ok, {_Key, NextHop}} ->
            case Payload of
                {error, ErrorMessage} ->
                    ?LOGGER:err("[~p]: ACTIVE - GENERATING RREP failed prepare payload: ~w.~n", [?MODULE, ErrorMessage]);
                _ ->
                    ?LOGGER:info("[~p]: ACTIVE - GENERATING RREP - NextHop: ~w.~n", [?MODULE, NextHop]),
                    ?DATA_LINK:send(StateData#state.bottom_level_pid, {{NextHop#routing_set_entry.medium, NextHop#routing_set_entry.next_addr}, Payload }),
                    report_management_message(Payload, StateData),
                    if ?ACK_REQUIRED -> %false by default - further implementations
                        add_new_entry_to_pending_acknowledgments(NextHop#routing_set_entry.next_addr,
                                                                 StateData#state.self_address,
                                                                 RREQSequenceNumber,
                                                                 StateData#state.pending_acknowledgements_set); %{next_hop, originator, r_seq_number, ack_received, ack_timeout}
                        true ->
                            ok
                end
            end;
        Error ->
            ?LOGGER:err("[~p]: ACTIVE - FAILED GENERATING RREP : ~w.~n", [?MODULE, Error])
    end,
    {next_state, active, StateData};


active({generate_rerr, {Destination, R_SEQ_NUMBER, ErrorCode, UnreacheableAddress}}, StateData) ->
    Payload = prepare_payload(StateData#state.self_address,
                              StateData#state.self_address,
                              Destination,
                              ?RERR,
                              [R_SEQ_NUMBER, StateData#state.self_address, UnreacheableAddress, ErrorCode]),
    Result = query_find_next_hop(Destination, StateData#state.routing_set), % {Medium, NextHopAddress}
    case Result of
        {ok, {_Key, NextHop}} ->
            case Payload of
                {error, ErrorMessage} ->
                    ?LOGGER:err("[~p]: ACTIVE - GENERATING RRER failed prepare payload: ~w.~n", [?MODULE, ErrorMessage]);
                _ ->
                    ?LOGGER:info("[~p]: ACTIVE - GENERATING RRER - NextHop: ~w.~n", [?MODULE, NextHop]),
                    ?DATA_LINK:send(StateData#state.bottom_level_pid, {{NextHop#routing_set_entry.medium, NextHop#routing_set_entry.next_addr}, Payload }),
                    report_management_message(Payload, StateData)
                end;
        Error ->
            ?LOGGER:err("[~p]: ACTIVE - FAILED GENERATING RRER : ~w.~n", [?MODULE, Error])
        end,
    {next_state, active, StateData};

active({generate_rack, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - GENERATING RACK for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(StateData#state.self_address,
                              StateData#state.self_address,
                              Destination,
                              ?RACK,
                              []),
    report_management_message(Payload, StateData),
    {next_state, active, StateData};




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Receive Messages Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
active({received_message, #load_ng_packet{type = ?DATA} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - DATA Packet : ~w .~n", [?MODULE, Packet]),
    update_routing_set_entry(Packet, StateData), % route maintanace
    case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
        true ->
            ?TRANSPORT:handle_incoming_message(StateData#state.upper_level_pid, Packet#load_ng_packet.data);
        false ->
            Result = forward_packet(Packet, StateData),
            case Result of
                {ok, sent, _Some } ->
                    ?LOGGER:debug("[~p]: DATA Packet successfully forwarded .~n", [?MODULE]),
                    report_data_message_received(Packet, StateData);
                {error, Error, #routing_set_entry{} = FailedHop} ->
                    ?LOGGER:debug("[~p]: DATA Packet FORWARDING ERROR: ~p , generating RERR towards ~p.~n", [?MODULE, Error, Packet#load_ng_packet.originator]),
                    generate_RERR({Packet#load_ng_packet.originator,
                                   FailedHop#routing_set_entry.r_seq_number,
                                   ?RERR_HOST_UNREACHABLE,
                                   Packet#load_ng_packet.destination});
               Else ->
                   ?LOGGER:critical("[~p]: ACTIVE - DATA NOT FORWARDED and RERR NOT GENERATED - Enexpected error: ~p .~n", [?MODULE, Else])
               end
    end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RREQ} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREQ RECEIVED : Packet : ~w .~n", [?MODULE, Packet]),
    IsValidPacket = isValidForProcessing(?RREQ, Packet, StateData),
    if  IsValidPacket ->
        %install reverse route
        add_new_entry_to_routing_set(StateData#state.routing_set,
                                     Packet#load_ng_packet.data#rreq_message.originator,
                                     Packet#load_ng_packet.source,
                                     Packet#load_ng_packet.medium,
                                     Packet#load_ng_packet.data#rreq_message.hop_count,
                                     Packet#load_ng_packet.data#rreq_message.r_seq_number),
        case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
            true ->
                RREQSequenceNumber = Packet#load_ng_packet.data#rreq_message.r_seq_number,
                Originator = Packet#load_ng_packet.data#rreq_message.originator,
                HopCount = Packet#load_ng_packet.data#rreq_message.hop_count,
                generate_RREP({Originator, RREQSequenceNumber, HopCount});
            false -> % this router is not a destination, forwarding message
                forward_packet(Packet, StateData),
                report_management_message(Packet, StateData)
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
        %TODO Remove rreq handling entry
        %install forward route
        add_new_entry_to_routing_set(StateData#state.routing_set,
                                     Packet#load_ng_packet.originator,
                                     Packet#load_ng_packet.source,
                                     Packet#load_ng_packet.medium,
                                     Packet#load_ng_packet.data#rrep_message.hop_count,
                                     Packet#load_ng_packet.data#rrep_message.r_seq_number),
        case amIDestination(Packet#load_ng_packet.destination, StateData#state.self_address) of
                true ->
                    %TODO generate RACK
                    ok;
                false ->
                    forward_packet(Packet, StateData),
                    report_management_message(Packet, StateData)
        end;
        true ->
          ?LOGGER:debug("[~p]: ACTIVE - RREP NOT VALID, Packet DROPPED.~n", [?MODULE])
        end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RERR} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RERR : Packet : ~w .~n", [?MODULE, Packet]),
    Result = remove_and_get_broken_link(Packet, StateData#state.routing_set),
    if Packet#load_ng_packet.originator =/= StateData#state.self_address ->
        case Result of
            {ok , RemovedEntry} ->
                forward_packet(Packet, StateData, RemovedEntry),
                report_management_message(Packet, StateData);
            {error , Message} ->
                ?LOGGER:err("[~p]: ACTIVE - RERR  - ERROR: ~p .~n", [?MODULE, Message]);
            Else ->
                ?LOGGER:critical("[~p]: ACTIVE - RERR  - Enexpected error: ~p .~n", [?MODULE, Else]),
                ok
        end;
        true ->
            ok
    end,
    {next_state, active, StateData};

active({received_message, #load_ng_packet{type = ?RACK} = Packet}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RACK : Packet : ~p .~n", [?MODULE, Packet]),
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


handle_sync_event(get_status, _From, StateName, State) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(get_status) ~n", [?MODULE]),
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#routing_set_entry.valid =:= true -> Entry end),
    RoutingSet = qlc:eval(ets:table(State#state.routing_set, [{traverse, {select, Query}}])),
    {reply, [{routing_set, RoutingSet}], StateName, State};


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
        {ok, {_Key, Hop}} -> Hop;
        {?EMPTY_QUERY_RESULT, _Message} ->
            ?LOGGER:debug("[~p]: get_next_hop NO HOP FOUND for destination: ~p : ~p.~n", [?MODULE, Destination, NextHop]),
            generate_RREQ(Destination),
            StartTime = get_current_millis(),
            receive after 2 * State#state.net_traversal_time ->
                NextHop2 = query_find_next_hop(Destination, State#state.routing_set),
                case NextHop2 of
                    {ok, {_Key, Hop}} -> Hop;
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
prepare_payload(Source, Originator, Destination, MessageType, Data)->
    %TODO Remove Headers - meaningless
    BinaryDestination = <<Destination:?ADDRESS_LENGTH>>,
    BinarySource = <<Source:?ADDRESS_LENGTH>>,
    BinaryOriginator = <<Originator:?ADDRESS_LENGTH>>,
    BinaryMessageType = <<MessageType:?MESSAGE_TYPE_LENGTH>>,
    BinaryData = list_to_binary(Data),
    BinaryDataLengthInBytes = byte_size(BinaryData),
    ?LOGGER:debug("[~p]: prepare_payload : MessageType: ~p, Originator: ~p, Source: ~p , Destination: ~p, DataLengthInBytes: ~p~n", [?MODULE,
                                                                                                                     BinaryMessageType,
                                                                                                                     BinaryOriginator,
                                                                                                                     BinarySource,
                                                                                                                     BinaryDestination,
                                                                                                                     BinaryDataLengthInBytes]),
    if (BinaryDataLengthInBytes =< ?MAX_DATA_LENGTH) ->
            Payload = <<BinaryMessageType/bitstring,
                        BinarySource/bitstring,
                        BinaryOriginator/bitstring,
                        BinaryDestination/bitstring,
                        BinaryDataLengthInBytes:?DATA_LENGTH_SIZE,
                        BinaryData/bitstring>>,
            ?LOGGER:debug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: ~p bytes , with maximum allowed: ~p ~n", [?MODULE,
                                                                                                                      BinaryDataLengthInBytes,
                                                                                                                      ?MAX_DATA_LENGTH]),
            {error, "Binary Data Length exceeded"}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Packet deserialization utilities %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
deserializePayload(Payload)->
    ?LOGGER:debug("[~p]: deserializePayload : Payload : ~w ~n", [?MODULE, Payload]),
    <<Type:?MESSAGE_TYPE_LENGTH,
      Source:?ADDRESS_LENGTH,
      Originator:?ADDRESS_LENGTH,
      Destination:?ADDRESS_LENGTH,
      DataLength:?DATA_LENGTH_SIZE,
      RestData/bitstring>> = Payload,
    DataLengthBits = DataLength * 8, % number of bit in data
    <<Data:DataLengthBits/bitstring, _Rest/bitstring>> = RestData,
    Packet = #load_ng_packet{
        type = Type,
        source = Source,
        originator = Originator,
        destination = Destination
    },
    deserializeMessage(Packet, Data).


deserializeMessage(#load_ng_packet{type = ?DATA} = Packet, Data)->
    Packet#load_ng_packet{data=binary_to_list(Data)};

deserializeMessage(#load_ng_packet{type = ?RREQ} = Packet, Data)->
    ?LOGGER:preciseDebug("[~p]: deserializeMessage RREQ : Data : ~w ~n", [?MODULE, Data]),
    RREQMessageData = binary_to_list(Data),
    RREQSequenceNumber = lists:nth(1, RREQMessageData),
    Originator = lists:nth(2, RREQMessageData),
    HopCount = lists:nth(3, RREQMessageData) + 1,

    RREQMessage = #rreq_message{
        originator = Originator,
        destination = Packet#load_ng_packet.destination,
        hop_count = HopCount,
        r_seq_number = RREQSequenceNumber
    },

    Packet#load_ng_packet{data = RREQMessage};

deserializeMessage(#load_ng_packet{type = ?RREP} = Packet, Data)->
    ?LOGGER:preciseDebug("[~p]: deserializeMessage RREP : Data : ~w ~n", [?MODULE, Data]),
    RREPMessageData = binary_to_list(Data),
    RREPSequenceNumber = lists:nth(1, RREPMessageData),
    Originator = lists:nth(2, RREPMessageData),
    HopCount = lists:nth(3, RREPMessageData) + 1,
    AckRequired = lists:nth(4, RREPMessageData),

    RREPMessage = #rrep_message{
        originator = Originator,
        destination = Packet#load_ng_packet.destination,
        hop_count = HopCount,
        r_seq_number = RREPSequenceNumber,
        ack_required = AckRequired
    },

    Packet#load_ng_packet{data = RREPMessage};

deserializeMessage(#load_ng_packet{type = ?RERR} = Packet, Data)->
    ?LOGGER:preciseDebug("[~p]: deserializeMessage RERR : Data : ~w ~n", [?MODULE, Data]),
    RERRMessageData = binary_to_list(Data),
    RERRSequenceNumber = lists:nth(1, RERRMessageData),
    Originator = lists:nth(2, RERRMessageData),
    UnreacheableAddress = lists:nth(3, RERRMessageData),
    ErrorCode = lists:nth(4, RERRMessageData),

    RERRMessage = #rerr_message{
        originator = Originator,
        destination = Packet#load_ng_packet.destination,
        unreachable_address = UnreacheableAddress,
        r_seq_number = RERRSequenceNumber,
        error_code = ErrorCode
    },

    Packet#load_ng_packet{data = RERRMessage};

deserializeMessage(#load_ng_packet{type = ?RACK} = Packet, Data)->
    ?LOGGER:preciseDebug("[~p]: deserializeMessage RACK : Data : ~w ~n", [?MODULE, Data]),
    RACKMessageData = binary_to_list(Data),
    RACKSequenceNumber = lists:nth(1, RACKMessageData),
    Originator = lists:nth(2, RACKMessageData),
    HopCount = lists:nth(3, RACKMessageData) + 1,

    RACKMessage = #rack_message{
        originator = Originator,
        destination = Packet#load_ng_packet.destination,
        hop_count = HopCount,
        r_seq_number = RACKSequenceNumber
    },

    Packet#load_ng_packet{data = RACKMessage}.

get_packet_data_as_list(?DATA, Data) ->
  Data;

get_packet_data_as_list(?RREQ, Data) ->
  [Data#rreq_message.r_seq_number, Data#rreq_message.originator, Data#rreq_message.hop_count ];

get_packet_data_as_list(?RREP, Data) ->
  [Data#rrep_message.r_seq_number, Data#rrep_message.originator, Data#rrep_message.hop_count,  Data#rrep_message.ack_required];

get_packet_data_as_list(?RERR, Data) ->
  [Data#rerr_message.r_seq_number, Data#rerr_message.originator, Data#rerr_message.unreachable_address, Data#rerr_message.error_code ];

get_packet_data_as_list(?RACK, Data) ->
  [Data#rack_message.r_seq_number, Data#rack_message.originator, Data#rack_message.hop_count].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

amIDestination(Destination, SelfAddress)->
    case Destination of
        SelfAddress ->
            true;
        _Else ->
            false
    end.

isValidForProcessing(Type, Packet, State) when Type =:= ?RREP ->
    isValidForProcessing(Packet#load_ng_packet.data#rrep_message.originator,
                         Packet#load_ng_packet.destination,
                         Packet#load_ng_packet.data#rrep_message.r_seq_number,
                         State);

isValidForProcessing(Type, Packet, State) when Type =:= ?RERR ->
    isValidForProcessing(Packet#load_ng_packet.data#rerr_message.originator,
                         Packet#load_ng_packet.destination,
                         Packet#load_ng_packet.data#rerr_message.r_seq_number,
                         State);

isValidForProcessing(Type, Packet, State) when Type =:= ?RACK ->
    isValidForProcessing(Packet#load_ng_packet.data#rack_message.originator,
                         Packet#load_ng_packet.destination,
                         Packet#load_ng_packet.data#rack_message.r_seq_number,
                         State);

isValidForProcessing(Type, Packet, State) when Type =:= ?RREQ ->
    isValidForProcessing(Packet#load_ng_packet.data#rreq_message.originator,
                         Packet#load_ng_packet.destination,
                         Packet#load_ng_packet.data#rreq_message.r_seq_number,
                         State).

isValidForProcessing(Originator, Destination, R_SEQ_NUMBER, State)->
    if  Originator =:= State#state.self_address -> false; %this router is the originator of request
       true ->
            case contains_is_rreq_handling_set(State#state.rreq_handling_set, {R_SEQ_NUMBER, Destination, Originator}) of %preventing flooding - RREQ will be not flooded , single retransmit
                false ->
                    case query_find_next_hop(Originator, State#state.routing_set) of
                      {ok, {_Key, Entry} } -> %found next hop (route exists)
                        IsGreater = compare_sequence_numbers(R_SEQ_NUMBER, Entry#routing_set_entry.r_seq_number),
                        if IsGreater ->
                            true;
                            true ->
                                    ?LOGGER:info("[~p]: isValidForProcessing : Routing Table already contains entry for originator ~p.~n", [?MODULE, Entry]),
                                false
                        end;
                      {?EMPTY_QUERY_RESULT, _Some} ->
                          ?LOGGER:debug("[~p]: isValidForProcessing - NO ENTRY FOUND.~n", [?MODULE]),
                          true;
                      {error, ErrorMessage} ->
                            ?LOGGER:err("[~p]: isValidForProcessing - error: ~p .~n", [?MODULE, ErrorMessage]),
                            false;
                      Else ->
                            ?LOGGER:critical("[~p]: isValidForProcessing - Enexpected error: ~p .~n", [?MODULE, Else]),
                            false
                    end;
                true ->
                    ?LOGGER:info("[~p]: isValidForProcessing : RREQ TIMEOUT EXPIRED.~n", [?MODULE]),
                    false
                end
    end.
%broadcast forwarding
forward_packet(#load_ng_packet{type = ?RREQ} = Packet, StateData) -> % only RREQ forwarded to BROADCAST_ADDRESS
    ?LOGGER:info("[~p]: forward_packet : RREQ in BROADCAST.~n", [?MODULE]),
    Payload = prepare_payload(StateData#state.self_address,
                              Packet#load_ng_packet.originator,
                              Packet#load_ng_packet.destination,
                              ?RREQ,
                              get_packet_data_as_list(Packet#load_ng_packet.type, Packet#load_ng_packet.data)),
    {Status, Message} = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload}),
    report_management_message(Payload, StateData),
    {Status, Message, {?RF_PLC, ?BROADCAST_ADDRESS}};

%unicast forwarding
forward_packet(Packet, StateData) ->
    {Status, Result} = query_find_next_hop(Packet#load_ng_packet.destination, StateData#state.routing_set), % {Medium, NextHopAddress}
    case Status of
        ok ->
            {_Key, NextHop} = Result,
            case NextHop#routing_set_entry.valid of
                true ->
                    forward_packet(Packet, StateData, NextHop);
                false ->
                    {error, hop_expired, NextHop}
                end;
        ?EMPTY_QUERY_RESULT ->
            {error, hop_not_found, Result};
        _Else ->
            {error, Result, Result}
    end.

forward_packet(Packet, StateData, NextHop) ->
    ?LOGGER:info("[~p]: forward_packet : to ~p, in UNICAST. Packet: ~w.~n", [?MODULE, NextHop, Packet]),
    Payload = prepare_payload(StateData#state.self_address,
                              Packet#load_ng_packet.originator,
                              Packet#load_ng_packet.destination,
                              Packet#load_ng_packet.type,
                              get_packet_data_as_list(Packet#load_ng_packet.type, Packet#load_ng_packet.data)),
    {SendStatus, Message} = ?DATA_LINK:send(StateData#state.bottom_level_pid, {{NextHop#routing_set_entry.medium, NextHop#routing_set_entry.next_addr}, Payload}),
    report_management_message(Payload, StateData),
    {SendStatus, Message, NextHop}.


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
        valid_time = get_current_millis() + ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS,
        valid = true
    },
    Result = ets:insert(RoutingSetId, {get_current_millis(), Entry}),
    ?LOGGER:info("[~p]: Routing Set updated with entry : ~w , Result : ~p .~n", [?MODULE, Entry, Result]),
    ?LOGGER:preciseDebug("[~p]: RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(RoutingSetId)]),

    Result.

query_find_next_hop(Destination, RoutingSetId)->
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#routing_set_entry.dest_addr =:= Destination -> {Key, Entry} end),
    NextHop = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    ?LOGGER:dev("[~p]: query_find_next_hop: Routing Set: ~n  ~p .~n", [?MODULE, ets:tab2list(RoutingSetId)]),
    ?LOGGER:dev("[~p]: query_find_next_hop: Destination: ~p .~n", [?MODULE, Destination]),

    Result = case NextHop of
        [Entry|[]] -> {ok, Entry};
        [_Entry|_Rest] ->
            {ok, get_route_set_entry_with_highest_seq_num(NextHop)};
        [] ->
            ?LOGGER:critical("[~p]: query_find_next_hop  NOT FOUND.~n", [?MODULE]),
            {?EMPTY_QUERY_RESULT, "NOT FOUND"};
        _ ->
            ?LOGGER:critical("[~p]: query_find_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, NextHop]),
            {error, "UNEXPECTED RESULTS ERROR"}
    end,
    Result.

add_new_entry_to_pending_acknowledgments(NextHop, Originator, RREQSequenceNumber, PendingAckSetId)-> %{next_hop, originator, r_seq_number, ack_received, ack_timeout}
    Entry = #pending_acknowledgement_entry{
        next_hop = NextHop,
        originator = Originator,
        r_seq_number = RREQSequenceNumber,
        ack_received = false,
        ack_timeout = get_current_millis() + ?NET_TRAVERSAL_TIME * 2
    },
    Result = ets:insert(PendingAckSetId, {get_current_millis(), Entry}),
    ?LOGGER:info("[~p]: Pending Acknowledges Set updated with entry : ~w , Result : ~p .~n", [?MODULE, Entry, Result]),
    ?LOGGER:preciseDebug("[~p]: PendingAckSet: ~n  ~p .~n", [?MODULE, ets:tab2list(PendingAckSetId)]),

    Result.

query_not_valid_routes(RoutingSetId)->
    CurrentMillis = get_current_millis(),
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#routing_set_entry.valid_time < CurrentMillis -> {Key, Entry} end),
    Result = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    Result.


query_expired_routes(RoutingSetId)->
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#routing_set_entry.valid =:= false -> {Key, Entry} end),
    Result = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    Result.

query_expired_rreq(RREQ_HandlingSet_Id)->
    CurrentMillis = get_current_millis(),
    Query = ets:fun2ms(fun({Key, Entry}) when Entry#rreq_handling_set_entry.valid_time < CurrentMillis -> {Key, Entry} end),
    Result = qlc:eval(ets:table(RREQ_HandlingSet_Id, [{traverse, {select, Query}}])),
    Result.

add_new_entry_to_rreq_handling_set(RREQ_HandlingSet_Id, {SeqNumber, Destination, Originator})->
    Entry = #rreq_handling_set_entry{r_seq_number = SeqNumber, destination = Destination, originator = Originator, valid_time = get_current_millis() + ?NET_TRAVERSAL_TIME * 2},
    Result = ets:insert(RREQ_HandlingSet_Id, {get_current_millis(), Entry}),
    ?LOGGER:info("[~p]: RREQ Handling Set updated with entry : ~w, Result : ~p .~n", [?MODULE, Entry, Result]),
    ?LOGGER:preciseDebug("[~p]: RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(RREQ_HandlingSet_Id)]),
    Result.

contains_is_rreq_handling_set(RREQ_HandlingSet_Id, {SeqNumber, Destination, Originator}) ->
    Query = ets:fun2ms(fun({_Key, Entry}) when (Entry#rreq_handling_set_entry.destination =:= Destination), (Entry#rreq_handling_set_entry.originator =:= Originator) -> Entry end),
    Result = qlc:eval(ets:table(RREQ_HandlingSet_Id, [{traverse, {select, Query}}])),
    case Result of
        [Entry|[]] ->
            NewSeqNumberIsGreater = compare_sequence_numbers(SeqNumber, Entry#rreq_handling_set_entry.r_seq_number),
            if  NewSeqNumberIsGreater ->
                false; % SeqNumber > Entry#rreq_handling_set_entry.r_seq_number
                true -> true
            end;
        [] ->
            false;
        Else ->
            ?LOGGER:critical("[~p]: contains_is_rreq_handling_set UNEXPECTED RESULTS: ~p.~n", [?MODULE, Else]),
            {error, "UNEXPECTED RESULTS ERROR in contains_is_rreq_handling_set"}
    end.

update_routing_set_entry(Packet, StateData) -> %{dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time}
    Query = ets:fun2ms(fun({Key, Entry}) when (Entry#routing_set_entry.dest_addr =:= Packet#load_ng_packet.destination),(Entry#routing_set_entry.valid =:= true) -> {Key, Entry} end),
    QueryResult = qlc:eval(ets:table(StateData#state.routing_set, [{traverse, {select, Query}}])),
    EntryToUpdate = case QueryResult of
        [H|[]] -> {ok, H};
        [_H|_T] -> {ok, get_route_set_entry_with_highest_seq_num(QueryResult)};
        [] ->
            {error, failed_updating_routing_set_entry_not_found };
        _ ->
            {error, failed_updating_routing_set_entry_unexpected_error}
    end,
    ?LOGGER:preciseDebug("[~p]: update_routing_set_entry RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(StateData#state.routing_set)]),
    ?LOGGER:preciseDebug("[~p]: update_routing_set_entry Destination: ~n  ~p .~n", [?MODULE, Packet#load_ng_packet.destination]),
    case EntryToUpdate of
        {ok, {Key, Entry}} ->
            ets:delete(StateData#state.routing_set, Key),
            ets:insert(StateData#state.routing_set, {Key, Entry#routing_set_entry{valid_time = get_current_millis() + ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS}}),
            ?LOGGER:info("[~p]: update_routing_set_entry : ~p.~n", [?MODULE, Entry]);
        {error, Mesage} ->
            ?LOGGER:err("[~p]: update_routing_set_entry ErrorMessage: ~p.~n", [?MODULE, Mesage]),
            {error, failed_updating_routing_set_entry}
    end.

remove_and_get_broken_link(Packet, StateData)->
    Query = ets:fun2ms(fun({Key, Entry}) when (Entry#routing_set_entry.dest_addr =:= Packet#load_ng_packet.data#rerr_message.unreachable_address),
                                              (Entry#routing_set_entry.next_addr =:= Packet#load_ng_packet.source),
                                              (Entry#routing_set_entry.r_seq_number =:= Packet#load_ng_packet.data#rerr_message.r_seq_number) ->  {Key, Entry} end),
    QueryResult = qlc:eval(ets:table(StateData#state.routing_set, [{traverse, {select, Query}}])),
    EntryToRemove = case QueryResult of
        [H|[]] -> {ok, H};
        [] ->
            {error, nothig_to_remove };
        _ ->
            {error, failed_updating_routing_set_entry_unexpected_error}
    end,

    case EntryToRemove of
        {ok, {Key, Entry}} ->
            ets:delete(StateData#state.routing_set, Key),
            ?LOGGER:info("[~p]: BROKEN LINK REMOVED : ~p.~n", [?MODULE, Entry]),
            ?LOGGER:preciseDebug("[~p]: RoutingSet: ~n  ~p .~n", [?MODULE, ets:tab2list(StateData#state.routing_set)]),
            {ok, Entry};
        {error, Mesage} ->
            ?LOGGER:err("[~p]: remove_broken_link_and_forward ErrorMessage: ~p.~n", [?MODULE, Mesage]),
            {error, failed_updating_routing_set_entry}
    end.


%----------------------------------------------------------------------------
% REPORT Functions
%----------------------------------------------------------------------------

%reporting process functions
report_message(Type, ReportingUnit, Message)->
    case ReportingUnit of
        undefined ->
            ?LOGGER:warn("[~p]: Reporting Unit is UNDEFINED.~n", [?MODULE]);
        _ ->
           ReportingUnit:report(Type, Message)
    end.

report_data_message_sent(Packet, State)->
    Data = [{?DATA_MESSAGE_TYPE, ?SEND_MESSAGE}, {data, Packet}],
    report_message(?DATA_MESSAGE, State#state.reporting_unit, Data).

report_data_message_received(Packet, State)->
    Data = [{?DATA_MESSAGE_TYPE, ?RECEIVED_MESSAGE}, {data, Packet}],
    report_message(?DATA_MESSAGE, State#state.reporting_unit, Data).

report_data_message_middle(Packet, State)->
    Data = [{?DATA_MESSAGE_TYPE, ?MIDDLE_MESSAGE}, {data, Packet}],
    report_message(?DATA_MESSAGE, State#state.reporting_unit, Data).

report_management_message(Packet, State)->
    Data = [{data, Packet}],
    report_message(?MANAGEMENT_MESSAGE, State#state.reporting_unit, Data).

%----------------------------------------------------------------------------
% UTILS Functions
%----------------------------------------------------------------------------
compare_sequence_numbers(S1, S2) ->
    MAXVALUE = ?SEQUENCE_NUMBER_MAX_VALUE - 1,
    Result = (((S2 < S1) and (S1 - S2 =< MAXVALUE/2)) or ((S1 < S2) and (S2 - S1 > MAXVALUE/2))),
    ?LOGGER:preciseDebug("[~p]: compare_sequence_numbers  ~p > ~p ? result : ~p .~n", [?MODULE, S1, S2, Result]),
    Result.


get_current_millis() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).


find_in_list(ComparatorFunction, [H|T])->
    lists:foldl(ComparatorFunction, H, T).

max_by_seq_num_route_set_entry_comparator({Key1, Entry1}, {Key2, Entry2})->
    case compare_sequence_numbers(Entry1#routing_set_entry.r_seq_number, Entry2#routing_set_entry.r_seq_number) of
        true -> {Key1, Entry1};
        false -> {Key2, Entry2}
    end.

get_route_set_entry_with_highest_seq_num(Entries) ->
    ?LOGGER:preciseDebug("[~p]: get_route_set_entry_with_highest_seq_num Entries: ~w .~n", [?MODULE, Entries]),
    Result = find_in_list(fun max_by_seq_num_route_set_entry_comparator/2, Entries),
    ?LOGGER:debug("[~p]: get_route_set_entry_with_highest_seq_num Result: ~w.~n", [?MODULE, Result]),
    Result.
