-module(load_ng_core).
-behaviour(gen_fsm).
-include("./include/properties.hrl").

-export([start/1, stop/1, updateBottomLevelPid/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

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


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {routing_set, bottom_level_pid}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Params) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Params]),
    RoutingSet_Id = ets:new(routing_set, [set, public]),
    {ok, idle, #state{
        routing_set = RoutingSet_Id
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

handle_sync_event({updateBottomLevelPid, BottomLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevelPid), StateName: ~p, StateData: ~p~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid},
    ?LOGGER:debug("[~p]: updateBottomLevelPid, StateName: ~p, NewState: ~p~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};


handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle SYNC EVENT Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Event, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData}.


handle_info(Request, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Request, StateName,StateData]),
    {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Event, StateName,StateData]),
    {next_state, normal, StateData}.

%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(Reason, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle TERMINATE Request, Reason: ~p, StateName: ~p, StateData: ~p~n", [?MODULE, Reason, StateName,StateData]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    ?LOGGER:debug("[~p]: STUB Handle CODE_CHANGE Request, OldVsn: ~p, StateName: ~p, StateData: ~p, Extra: ~p.~n", [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.


