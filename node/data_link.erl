-module(data_link).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-export([init/1]).

-export([handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/1, stop_fsm/1]).


start_fsm(Params) ->
	gen_fsm:start(?MODULE, Params, []).

stop_fsm(Ref)->
	gen_fsm:send_all_state_event(Ref, stop).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {routing_set}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Params) ->
    RoutingSet_Id = ets:new(routing_set, [set, public]),
    {ok, idle, #state{
        routing_set = RoutingSet_Id
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOGGER:debug("~p STUB Handle SYNC EVENT Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Event, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData}.


handle_info(Request, StateName, StateData) ->
    ?LOGGER:debug("~p STUB Handle INFO Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Request, StateName,StateData]),
    {next_state, StateName, StateData}.

handle_event(Event, StateName, StateData) ->
    ?LOGGER:debug("~p STUB Handle INFO Request(~p), StateName: ~p, StateData: ~p~n", [?MODULE, Event, StateName,StateData]),
    {next_state, normal, StateData}.

%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(Reason, StateName, StateData) ->
    ?LOGGER:debug("~p STUB Handle TERMINATE Request, Reason: ~p, StateName: ~p, StateData: ~p~n", [?MODULE, Reason, StateName,StateData]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    ?LOGGER:debug("~p STUB Handle CODE_CHANGE Request, OldVsn: ~p, StateName: ~p, StateData: ~p, Extra: ~p.~n", [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.



