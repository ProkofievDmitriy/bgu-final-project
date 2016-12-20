-module(load_ng_core).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/records.hrl").


%include libraries to support qlc requests
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2, send/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
%states export
-export([idle/3, idle/2]).

-record(state, {routing_set,rreq_handling_set, bottom_level_pid, net_traversal_time}).
-record(load_ng_message, {medium, source, destination, tlv, payload}).
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

send(FsmPid, {Destination, Headers, Data})->
    %default sync event timeout 5000ms
    gen_fsm:sync_send_event(FsmPid, {send_message, {Destination, Headers, Data}}).

%% ====================================================================
%% Internal events
%% ====================================================================
generate_RREQ(Destination)->
    gen_fsm:send_event(self(), {generate_rreq, Destination}).


%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Properties]),
    NetTraversalTime = proplists:get_value(net_traversal_time, Properties),
    RoutingSet_Id = ets:new(routing_set, [set, public]),
    RREQHandlingSet_Id = ets:new(rreq_handling_set, [set, public]),
    {ok, idle, #state{
        routing_set = RoutingSet_Id,
        rreq_handling_set = RREQHandlingSet_Id,
        net_traversal_time = NetTraversalTime
    }}.

%% ============================================================================================
%% =========================================== SYNC States Transitions ========================
%% ============================================================================================


idle({send_message, {Destination, Headers, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Request(send_message) in idle state, Message: {~p, ~p, ~p},  StateData: ~w~n", [?MODULE, Destination, Headers, Data, StateData]),
    NextHop = get_next_hop(Destination, StateData), % {Medium, NextHopAddress}
    case NextHop of
        {error, Message} ->
            {reply, {error, Message}, idle, StateData};
        Hop ->
            Payload = prepare_payload(Destination, Headers, Data),
            ?DATA_LINK:send(StateData#state.bottom_level_pid, {Hop, {Payload}}),
            {reply, sent, idle, StateData}
    end.
%% ============================================================================================
%% =========================================== A-SYNC States Transitions ========================
%% ============================================================================================


idle({generate_rreq, Destination}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Generating RREQ for ~p.~n", [?MODULE, Destination]),
    {next_state, idle, StateData};

idle({rreq_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - RREQ RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, idle, StateData};

idle({rreq_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - RREP RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, idle, StateData};

idle({rreq_received, Message}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - RERR RECEIVED : ~p.~n", [?MODULE, Message]),
    {next_state, idle, StateData}.

%% ============================================================================================
%% =========================================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateBottomLevelPid, BottomLevelPid}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid},
    ?LOGGER:debug("[~p]: updateBottomLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
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
    NextHop = find_next_hop(Destination, State#state.routing_set),
    Result = case NextHop of
        {ok, Hop} -> Hop;
        {?EMPTY_QUERY_RESULT, _Message} ->
            ?LOGGER:debug("[~p]: get_next_hop NO HOP FOUND for destination: ~p : ~p.~n", [?MODULE, Destination, NextHop]),
            generate_RREQ(Destination),
            receive after 2 * State#state.net_traversal_time ->
                NextHop = find_next_hop(Destination, State#state.routing_set),
                case NextHop of
                    {ok, Hop} -> Hop;
                    _ ->
                        ?LOGGER:err("[~p]: get_next_hop TIMEOUT EXCEEDED.~n", [?MODULE]),
                        {error, "TIME OUT EXCEEDED"}
                end
            end;
        Error ->
            ?LOGGER:err("[~p]: get_next_hop UNEXPECTED RESULTS: ~p.~n", [?MODULE, NextHop]),
            Error
    end,
    ?LOGGER:debug("[~p]: get_next_hop Result: ~p.~n", [?MODULE, Result]),
    Result.



prepare_payload(Destination, Headers, Data)->
    Payload = [Destination] ++ Headers ++ [Data],
    ?LOGGER:debug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
    Payload.

%----------------------------------------------------------------------------
% DATA QUERIES
%----------------------------------------------------------------------------

find_next_hop(Destination, RoutingSetId)->
    Query = ets:fun2ms(fun({{Medium, Address},_Value}) when Address =:= Destination -> {Medium, Address} end),
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