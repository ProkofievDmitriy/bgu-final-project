-module(data_link).
-behaviour(gen_fsm).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-export([start/1, stop/1, send/2, updateUpperLevelPid/2, enable_plc/1, enable_rf/1, disable_plc/1, disable_rf/1, disable/1, enable/1, handle_incoming_message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export.
-export([idle/3, dual/3, dual/2, plc_only/3, rf_only/3]).
%% ====================================================================
%% API functions
%% ====================================================================


start(Params) ->
	{ok,PID} = gen_fsm:start(?MODULE, Params, []),
	PID.

stop(FsmPid)->
	gen_fsm:send_all_state_event(FsmPid, stop).

%Controlling events
enable_plc(FsmPid)->
    gen_fsm:send_event(FsmPid, enable_plc).

enable_rf(FsmPid)->
    gen_fsm:send_event(FsmPid, enable_rf).

disable_plc(FsmPid)->
    gen_fsm:send_event(FsmPid, disable_plc).

disable_rf(FsmPid)->
    gen_fsm:send_event(FsmPid, disable_rf).

disable(FsmPid)->
    gen_fsm:send_event(FsmPid, disable).

enable(FsmPid)->
    gen_fsm:send_event(FsmPid, enable).
%Managing events
send(FsmPid, {Hop, Payload})->
    gen_fsm:sync_send_event(FsmPid, {send, {Hop, Payload}}).

updateUpperLevelPid(FsmPid, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevelPid, UpperLevelPid}).

handle_incoming_message(FsmPid, Packet)->
    <<Medium:8, RSSI:8, Target:?ADDRESS_LENGTH, Data/bitstring>> = list_to_binary(Packet), % parse incoming packet, currently ignore RSSI
    ?LOGGER:debug("[~p]: handle_incoming_message : Medium: ~p , RSSI: ~p, Target: ~p, Data : ~w ~n", [?MODULE, Medium, RSSI, Target, Data]),
    gen_fsm:send_event(FsmPid, {received_message, {Medium, Target, Data}}).



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
    StartState = proplists:get_value(default_state, Properties),

    {ok, StartState, #state{
        self_address = SelfAddress
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% =========================================== IDLE =========================================
idle(enable_plc, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(enable_plc) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, plc_only, StateData};

idle(enable_rf, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(enable_rf) , StateData: ~w~n", [?MODULE, StateData]),
    {reply, ok, rf_only, StateData};

idle(enable, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, dual, StateData}.


%% =========================================== DUAL =========================================
dual(disable, _From, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, idle, StateData};

dual(disable_plc, _From, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - Event(disable_plc) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, rf_only, StateData};

dual(disable_rf, _From, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - Event(disable_rf) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, plc_only, StateData};

dual({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - Event(send) to medium ~p, StateData: ~w~n", [?MODULE, Hop, StateData]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data), % <<NextHopAddress/bitstring, Data/bitstring>>,
    ?MODEM_PORT:send(Medium, Payload),
	{reply, ok, dual, StateData}.


% Async dual events
dual({received_message, {Medium, Target, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - Event(received_message), Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Medium, Target, Data]),
    handle_message(Target, StateData, Data),
    {next_state, dual, StateData}.



%% =========================================== PLC ONLY =========================================
plc_only(disable, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, idle, StateData};

plc_only(disable_plc, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Event(disable_plc) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, idle, StateData};

plc_only(enable_rf, _From, StateData) ->
     ?LOGGER:debug("[~p]: PLC_ONLY - Event(enable_rf) , StateData: ~w~n", [?MODULE, StateData]),
    {reply, ok, dual, StateData};

plc_only(enable, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, dual, StateData};


plc_only({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Request(send) to ~p, StateData: ~w~n", [?MODULE, Hop, StateData]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data), % <<NextHopAddress/bitstring, Data/bitstring>>,
    if Medium == ?PLC ->
            ?MODEM_PORT:send(Medium, Payload),
	        {reply, ok, plc_only, StateData};
	    true ->
	        {reply, {error, not_active_medium}, plc_only, StateData}
	 end.

plc_only({received_message, {Medium, Target, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Event(received_message), Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Medium, Target, Data]),
    case Medium of
        ?PLC ->
            handle_message(Target, StateData, Data);
        _Else ->
            ?LOGGER:debug("[~p]: PLC_ONLY - Event(received_message) : Medium is NOT PLC - IGNORING incoming message ~n", [?MODULE])
    end,
    {next_state, plc_only, StateData}.


%% =========================================== RF ONLY =========================================
rf_only(disable, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - Event(disable) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, idle, StateData};

rf_only(disable_rf, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - Event(disable_rf) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, idle, StateData};

rf_only(enable_plc, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - Event(enable_plc) , StateData: ~w~n", [?MODULE, StateData]),
     {reply, ok, dual, StateData};

rf_only(enable, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - Event(enable) , StateData: ~w~n", [?MODULE, StateData]),
    {reply, ok, dual, StateData};

rf_only({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - Request(send) to medium ~p, StateData: ~w~n", [?MODULE, Hop, StateData]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data), % <<NextHopAddress/bitstring, Data/bitstring>>,
    if Medium == ?RF ->
            ?MODEM_PORT:send(Medium, Payload),
	        {reply, ok, plc_only, StateData};
	    true ->
	        {reply, {error, not_active_medium}, plc_only, StateData}
	 end.

rf_only({received_message, {Medium, Target, Data}}, StateData) ->
     ?LOGGER:debug("[~p]: RF_ONLY - Event(received_message), Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Medium, Target, Data]),
     case Medium of
         ?PLC ->
             handle_message(Target, StateData, Data);
         _Else ->
             ?LOGGER:debug("[~p]: RF_ONLY - Event(received_message) : Medium is NOT RF - IGNORING incoming message ~n", [?MODULE])
     end,
     {next_state, rf_only, StateData}.

%% ============================================================================================
%% ============================== Sync Event Handling =========================================
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
preparePayload(Address, Data)->
    BinaryNextHopAddress = <<Address:?ADDRESS_LENGTH>>,
    Payload = <<BinaryNextHopAddress/bitstring, Data/bitstring>>,
    if (bit_size(Payload) =< ?MAX_FRAME_LENGTH) ->
            ?LOGGER:debug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: byte_size : ~p, bit_size: ~p ~n", [?MODULE, byte_size(Payload), bit_size(Payload)]),
            {error, "Binary Data Length exceeded"}
    end.

isValidTarget(Target, SelfAddress)->
     case Target of
        SelfAddress ->
            true;
        ?BROADCAST_ADDRESS ->
            true;
        _Else ->
            false
         end.

handle_message(Target, StateData, Data)->
    case isValidTarget(Target, StateData#state.self_address) of
        true ->
            ?LOGGER:debug("[~p]: handle_message : target is valid forwarding to network layer~n", [?MODULE]),
            ?NETWORK:handle_incoming_message(StateData#state.upper_level_pid, Data);
        Else ->
            ?LOGGER:debug("[~p]: handle_message : target is NOT valid - IGNORING incoming message ~n", [?MODULE])
    end.