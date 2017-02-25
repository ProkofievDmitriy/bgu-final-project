-module(load_ng_core).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2, updateUpperLevelPid/2, send/2, enable/1, disable/1]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export
-export([active/3, active/2, idle/3]).

-record(state, {routing_set,rreq_handling_set, self_address, address_length, bottom_level_pid, upper_level_pid, net_traversal_time, reporting_unit}).
-record(load_ng_message, {medium, type, source, destination, tlv, payload}).
-record(routing_set_entry, {medium, destination, next_hop, link_cost, last_used}).
-record(rreq_handling_set_entry, {rreq_id, destination, created_time}).
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


handle_message(FsmPid, MessageType, Message)->
    gen_fsm:send_event(FsmPid, {MessageType, Message}).


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
        self_address = SelfAddress
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
            Payload = prepare_payload(Destination, ?DATA, Data), %% <<Destination/bitstring, MessageType/bitstring, Data/bitstring>>
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
    Payload = prepare_payload(Destination, ?RREQ, []),
    case Payload of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: ACTIVE - Generating RREQ failed prepare payload: ~p.~n", [?MODULE, ErrorMessage]),
            %TODO handle error on a-synch events
           {next_state, active, StateData};
        _ ->
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {{?RF_PLC, ?BROADCAST_ADDRESS}, Payload }),
            report_management_message(StateData#state.reporting_unit, Payload),
            {next_state, active, StateData}
    end;

active({generate_rrep, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RREP for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(Destination, ?RREP, []),
    report_management_message(StateData#state.reporting_unit, Payload),
    {next_state, active, StateData};

active({generate_rerr, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - Generating RRER for ~p.~n", [?MODULE, Destination]),
    Payload = prepare_payload(Destination, ?RERR, []),
    report_management_message(StateData#state.reporting_unit, Payload),
    {next_state, active, StateData};


% Receive Messages Handlers
active({rreq_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREQ RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, active, StateData};

active({rrep_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RREP RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, active, StateData};

active({rerr_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - RERR RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, active, StateData};

active({data_message_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: ACTIVE - DATA RECEIVED : ~p.~n", [?MODULE, Message]),
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



prepare_payload(Destination, MessageType, Data)->
    %TODO Remove Headers - meaningless
    BinaryDestination = <<Destination:?ADDRESS_LENGTH>>,
    BinaryMessageType = <<MessageType:?MESSAGE_TYPE_LENGTH>>,
    BinaryData = list_to_binary(Data),
    BinaryDataLengthInBits = bit_size(BinaryData),
    ?LOGGER:debug("[~p]: prepare_payload BinaryDestination: ~p , BinaryMessageType: ~p, BinaryData: ~p, BinaryDataLengthInBits: ~p.~n", [?MODULE, BinaryDestination, BinaryMessageType, BinaryData ,BinaryDataLengthInBits]),
    if (BinaryDataLengthInBits =< ?MAX_DATA_LENGTH) ->
            Payload = <<BinaryDestination/bitstring, BinaryMessageType/bitstring, BinaryData/bitstring>>,
            ?LOGGER:debug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: bit_size: ~p ~n", [?MODULE, BinaryDataLengthInBits]),
            {error, "Binary Data Length exceeded"}
    end.




%----------------------------------------------------------------------------
% DATA QUERIES
%----------------------------------------------------------------------------
update_routing_set(RoutingSetId, Destination, NextHop, Medium)->
    Result = ets:insert(RoutingSetId, {Destination, {Medium, NextHop}}),
    ?LOGGER:info("[~p]: Routing Set updated with : {~p, {~p, ~p}} , Result : ~p .~n", [?MODULE, Destination, Medium, NextHop, Result]),
    Result.


query_find_next_hop(Destination, RoutingSetId)->
    Query = ets:fun2ms(fun({Address, NextHop}) when Address =:= Destination -> NextHop end),
    NextHop = qlc:eval(ets:table(RoutingSetId, [{traverse, {select, Query}}])),
    Result = case NextHop of
        [Hop|[]] -> {ok, Hop};
        [] ->
            {?EMPTY_QUERY_RESULT, "NOT FOUND"};
        _ ->
            ?LOGGER:err("[~p]: find_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, NextHop]),
            {error, "UNEXPECTED RESULTS ERROR"}
    end,
    Result.

update_rreq_handling_set(RREQ_HandlingSet_Id, RREQ_Message)->



ok.


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

