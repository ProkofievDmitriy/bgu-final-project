-module(data_link).
-behaviour(gen_fsm).
-include("./include/properties.hrl").

-export([start/1, stop/1, send/2, updateUpperLevelPid/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================


start(Params) ->
	{ok,PID} = gen_fsm:start(?MODULE, Params, []),
	PID.

stop(FsmPid)->
	gen_fsm:send_all_state_event(FsmPid, stop).

send(FsmPid, {Medium, Payload})->
    gen_fsm:send_event(FsmPid, {send, {Medium, Payload}}).

updateUpperLevelPid(FsmPid, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevelPid, UpperLevelPid}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {properties, my_address, upper_level_pid}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Properties]),
    MyAddress = proplists:get_value(address, Properties),
    {ok, idle, #state{
        my_address = MyAddress,
        properties = Properties
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

handle_sync_event({send, {Medium, Payload}}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(send), StateName: ~p, StateData: ~p~n", [?MODULE, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData};

handle_sync_event({updateUpperLevelPid, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevelPid), StateName: ~p, StateData: ~p~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid},
    ?LOGGER:debug("[~p]: updateUpperLevelPid, StateName: ~p, NewState: ~p~n", [?MODULE, StateName, NewState]),
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
    %TODO Proper terminate with all consequences
    ?LOGGER:debug("[~p]: STUB Handle TERMINATE Request, Reason: ~p, StateName: ~p, StateData: ~p~n", [?MODULE, Reason, StateName,StateData]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    ?LOGGER:debug("[~p]: STUB Handle CODE_CHANGE Request, OldVsn: ~p, StateName: ~p, StateData: ~p, Extra: ~p.~n", [?MODULE, OldVsn, StateName, StateData, Extra]),
    {ok, StateName, StateData}.



