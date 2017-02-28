-module(transport).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-export([start/1, stop/1, send/2, updateUpperLevelPid/2, updateBottomLevelPid/2, disable/1, enable/1, handle_incoming_message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export.
-export([idle/3, disable/3, idle/2, disable/2]).
%% ====================================================================
%% API functions
%% ====================================================================


start(Params) ->
    Timeout = proplists:get_value(timeout, Params),
    {ok,PID} = gen_fsm:start(?MODULE, Params, [{timeout, Timeout}]),
	PID.

stop(FsmPid)->
	gen_fsm:send_all_state_event(FsmPid, stop).

%Controlling events
enable(FsmPid)->
    gen_fsm:send_event(FsmPid, enable_plc).

disable(FsmPid)->
    gen_fsm:send_event(FsmPid, disable_plc).

%Managing events
send(FsmPid, {Destination, Data})->
    gen_fsm:sync_send_event(FsmPid, {send, {Destination, Data}}, ?NET_TRAVERSAL_TIME * 3).

updateUpperLevelPid(FsmPid, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevelPid, UpperLevelPid}).

updateBottomLevelPid(FsmPid, BottomLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateBottomLevelPid, BottomLevelPid}).

handle_incoming_message(FsmPid, Message)->
    gen_fsm:send_event(FsmPid, {received_message, Message}).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {upper_level_pid, bottom_level_pid}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Properties]),
    StartState = proplists:get_value(default_state, Properties),

    {ok, StartState, #state{}}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% =========================================== IDLE =========================================
idle({received_message, Message}, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(received_message) , StateData: ~w~n", [?MODULE, StateData]),
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
    ?LOGGER:debug("[~p]: IDLE - Event(send) , {Destination, Data} : {~p, ~p}, StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
     {reply, ok, idle, StateData}.


%% =========================================== DISABLE =========================================
%% Pass all message as is - no session management enabled
disable({received_message, Message}, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(received_message) , StateData: ~w~n", [?MODULE, StateData]),
    StateData#state.upper_level_pid ! binary:bin_to_list(Message),
    {next_state, disable, StateData};

disable(disable, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
    {next_state, disable, StateData};

disable(enable, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
     {next_state, idle, StateData}.

%Synchronous event call
disable({send, {Destination, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: DISABLE - Event(send) , {Destination, Data} : {~p, ~p}, StateData: ~w~n", [?MODULE, Destination, Data, StateData]),
    ?NETWORK:send(StateData#state.bottom_level_pid, {Destination, Data}),
     {reply, ok, disable, StateData}.


%% ============================================================================================
%% ============================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateUpperLevelPid, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid},
    ?LOGGER:debug("[~p]: updateUpperLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

handle_sync_event({updateBottomLevelPid, BottomLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid},
    ?LOGGER:debug("[~p]: updateBottomLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

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
