-module(data_link).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-export([start/1, stop/1, send/2, updateUpperLevelPid/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export.
-export([double/3, plc_only/3, rf_only/3]).
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
-record(state, {self_address, upper_level_pid}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Properties]),
    SelfAddress = proplists:get_value(?SELF_ADDRESS, Properties),
    {ok, idle, #state{
        self_address = SelfAddress
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

double({send, {Medium, Payload}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: DOUBLE - Request(send) to medium ~p, StateData: ~w~n", [?MODULE, Medium, StateData]),
    ?MODEM_PORT:send(Medium, Payload),
	{reply, ok, double, StateData}.

plc_only({send, {Medium, Payload}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC ONLY - Request(send) to medium ~p, StateData: ~w~n", [?MODULE, Medium, StateData]),
    case Medium == ?PLC of
        true ->
            ?MODEM_PORT:send(Medium, Payload),
	        {reply, ok, plc_only, StateData};
	    _ ->
	        {reply, error, plc_only, StateData}
	 end.

rf_only({send, {Medium, Payload}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF ONLY - Request(send) to medium ~p, StateData: ~w~n", [?MODULE, Medium, StateData]),
    ?MODEM_PORT:send(Medium, Payload),
	{reply, ok, idle, StateData}.


%% ============================================================================================
%% =========================================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateUpperLevelPid, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevelPid), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid},
    ?LOGGER:debug("[~p]: updateUpperLevelPid, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle SYNC EVENT Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Event, StateName, StateData]),
	{reply, "Stub Reply", StateName, StateData}.

%% ============================================================================================
%% =========================================== INFO Event Handling =========================================
%% ============================================================================================
handle_info(Request, StateName, StateData) ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), StateName: ~p, StateData: ~w~n", [?MODULE, Request, StateName,StateData]),
    {next_state, StateName, StateData}.
%% ============================================================================================
%% =========================================== A-Sync Event Handling =========================================
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



