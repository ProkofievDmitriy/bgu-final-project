-module(data_link).
-behaviour(gen_fsm).
-behaviour(layer_interface).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/macros.hrl").

-export([start/1, stop/1, send/3, send_async/3, updateUpperLevel/3, updateBottomLevel/3, handle_incoming_message/2, get_status/1, set_state/2, update_nodes_to_filter/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%states export.
-export([idle/3, dual/3, dual/2, plc_only/3, rf_only/3, plc_only/2, rf_only/2]).
%% ====================================================================
%% API functions

start(Params) ->
    Timeout = proplists:get_value(timeout, Params),
	{ok,PID} = gen_fsm:start(?MODULE, Params, [{timeout, Timeout}]),
	PID.

stop(FsmPid)->
	gen_fsm:send_all_state_event(FsmPid, stop).

%Controlling events
set_state(FsmPid, NewState)->
    gen_fsm:sync_send_all_state_event(FsmPid, {set_state, NewState}).

update_nodes_to_filter(FsmPid, NodesToFilter)->
    gen_fsm:sync_send_all_state_event(FsmPid, {update_nodes_to_filter, NodesToFilter}).

%Managing events
send(FsmPid, Hop, Payload)->
    gen_fsm:sync_send_event(FsmPid, {send, {Hop, Payload}}, ?TIMEOUT).

%Managing events
send_async(FsmPid, Hop, Payload)->
    gen_fsm:send_event(FsmPid, {send, {Hop, Payload}}).


updateUpperLevel(FsmPid, UpperLevelModule, UpperLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateUpperLevel, UpperLevelModule, UpperLevelPid}).

updateBottomLevel(FsmPid, BottomLevelModule, BottomLevelPid)->
    gen_fsm:sync_send_all_state_event(FsmPid, {updateBottomLevel, BottomLevelModule, BottomLevelPid}).



handle_incoming_message(FsmPid, Packet)->
    ?LOGGER:preciseDebug("[~p]: handle_incoming_message : Packet: ~w ~n", [?MODULE, Packet]),
    <<Medium:8, RSSI:8, Source:?ADDRESS_LENGTH, Target:?ADDRESS_LENGTH, Data/bitstring>> = list_to_binary(Packet), % parse incoming packet, currently ignore RSSI
    ?LOGGER:debug("[~p]: handle_incoming_message : Medium: ~p , RSSI: ~p, Target: ~p, Data : ~w ~n", [?MODULE, ?GET_MEDIUM_NAME(Medium), RSSI, Target, Data]),
    gen_fsm:send_event(FsmPid, {received_message, {Medium, Source, Target, Data}}).

get_status(FsmPid) ->
    Result = (catch gen_fsm:sync_send_all_state_event(FsmPid, get_status, 60000)),
    if is_list(Result) ->
        Result;
        true ->
            ?LOGGER:critical("[~p]: error occured while get_status, Result = ~p ~n", [?MODULE, Result]),
            []
    end.



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {self_address, upper_level_pid, upper_level_module, bottom_level_pid, bottom_level_module, nodes_to_filter}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init(Properties) ->
    ?LOGGER:info("[~p]: Starting FSM with params: ~p.~n", [?MODULE, Properties]),
    SelfAddress = proplists:get_value(?SELF_ADDRESS, Properties),
    NodesToFilter = proplists:get_value(?NODES_TO_FILTER, Properties),
    StartState = proplists:get_value(default_state, Properties),

    {ok, StartState, #state{
        self_address = SelfAddress,
        nodes_to_filter = NodesToFilter
    }}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% =========================================== IDLE =========================================

idle(Event, _From, StateData) ->
    ?LOGGER:debug("[~p]: IDLE - (~p) IGNORING EVENT, StateData: ~w~n", [?MODULE, Event, StateData]),
     {reply, ok, dual, StateData}.


%% =========================================== DUAL =========================================

dual({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - (send) to medium ~p, StateData: ~w~n", [?MODULE, Hop, StateData]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
    Result = ?MODEM_PORT:send(Medium, Payload),
    case Result of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
	        {reply, ErrorMessage, dual, StateData};
	    _ ->
	        {reply, Result, dual, StateData}
	end.


% Async dual events
dual({received_message, {Medium, Source, Target, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - (received_message),Source: ~p, Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Source, Medium, Target, Data]),
    handle_message(Medium, Source, Target, StateData, Data),
    {next_state, dual, StateData};


dual({send, {Hop, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: DUAL - ASYNC  (send) to medium ~p, StateData: ~w~n", [?MODULE, Hop, StateData]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
    Result = ?MODEM_PORT:send(Medium, Payload),
    case Result of
        {error, ErrorMessage} ->
            ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
            {next_state, dual, StateData};
	    _ ->
            {next_state, dual, StateData}
	end.

%% =========================================== PLC ONLY =========================================


plc_only({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - Request(send) to ~p~n", [?MODULE, Hop]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
    Result = case Medium of
        ?PLC ->
            ?MODEM_PORT:send(?PLC, Payload);
        ?RF_PLC ->
            ?MODEM_PORT:send(?PLC, Payload);
	    _Else ->
	        {error, not_active_medium}
	 end,

     case Result of
         {error, ErrorMessage} ->
             ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
            {reply, ErrorMessage, dual, StateData};
        _ ->
            {reply, Result, plc_only, StateData}
     end.

plc_only({received_message, {Medium, Source, Target, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - (received_message), Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Medium, Target, Data]),
    case Medium of
        ?PLC ->
            handle_message(Medium, Source, Target, StateData, Data);
        _Else ->
            ?LOGGER:warn("[~p]: PLC_ONLY - (received_message) : Medium is NOT PLC - IGNORING incoming message ~n", [?MODULE])
    end,
    {next_state, plc_only, StateData};



plc_only({send, {Hop, Data}}, StateData) ->
    ?LOGGER:debug("[~p]: PLC_ONLY - ASYNC (send) to ~p~n", [?MODULE, Hop]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
    Result = case Medium of
        ?PLC ->
            ?MODEM_PORT:send(?PLC, Payload);
        ?RF_PLC ->
            ?MODEM_PORT:send(?PLC, Payload);
	    _Else ->
	        {error, not_active_medium}
	 end,

     case Result of
         {error, ErrorMessage} ->
             ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
             {next_state, plc_only, StateData};
        _ ->
            {next_state, plc_only, StateData}
     end.


%% =========================================== RF ONLY =========================================

rf_only({send, {Hop, Data}}, _From, StateData) ->
    ?LOGGER:debug("[~p]: RF_ONLY - (send) to medium ~p~n", [?MODULE, Hop]),
    {Medium, NextHopAddress} = Hop,
    Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
    Result = case Medium of
        ?RF ->
            ?MODEM_PORT:send(?RF, Payload);
        ?RF_PLC ->
            ?MODEM_PORT:send(?RF, Payload);
	    _Else ->
	        {error, not_active_medium}
	 end,

     case Result of
         {error, ErrorMessage} ->
             ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
            {reply, ErrorMessage, dual, StateData};
        _ ->
            {reply, Result, rf_only, StateData}
     end.

rf_only({received_message, {Medium, Source, Target, Data}}, StateData) ->
     ?LOGGER:debug("[~p]: RF_ONLY - (received_message), Medium: ~p , Target: ~p, Data : ~w ~n", [?MODULE, Medium, Target, Data]),
     case Medium of
         ?RF ->
             handle_message(Medium, Source, Target, StateData, Data);
         _Else ->
             ?LOGGER:warn("[~p]: RF_ONLY - (received_message) : Medium is NOT RF - IGNORING incoming message ~n", [?MODULE])
     end,
     {next_state, rf_only, StateData};


 rf_only({send, {Hop, Data}}, StateData) ->
     ?LOGGER:debug("[~p]: RF_ONLY -  ASYNC (send) to medium ~p~n", [?MODULE, Hop]),
     {Medium, NextHopAddress} = Hop,
     Payload = preparePayload(NextHopAddress, Data, StateData#state.self_address), % <<NextHopAddress/bitstring, Data/bitstring>>,
     Result = case Medium of
         ?RF ->
             ?MODEM_PORT:send(?RF, Payload);
         ?RF_PLC ->
             ?MODEM_PORT:send(?RF, Payload);
 	    _Else ->
 	        {error, not_active_medium}
 	 end,

      case Result of
          {error, ErrorMessage} ->
              ?LOGGER:err("[~p]: Error received from modem port : ~p~n",[?MODULE, ErrorMessage]),
              {next_state, rf_only, StateData};
         _ ->
             {next_state, rf_only, StateData}
      end.

%% ============================================================================================
%% ============================== Sync Event Handling =========================================
%% ============================================================================================

handle_sync_event({updateUpperLevel, UpperLevelModule, UpperLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateUpperLevel), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{upper_level_pid = UpperLevelPid, upper_level_module=UpperLevelModule},
    ?LOGGER:debug("[~p]: updateUpperLevel, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

handle_sync_event({updateBottomLevel, BottomLevelModule, BottomLevelPid }, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle SYNC EVENT Request(updateBottomLevel), StateName: ~p, StateData: ~w~n", [?MODULE, StateName, StateData]),
    NewState = StateData#state{bottom_level_pid = BottomLevelPid, bottom_level_module=BottomLevelModule},
    ?LOGGER:debug("[~p]: updateBottomLevel, StateName: ~p, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, StateName, NewState};

handle_sync_event(get_status, _From, StateName, StateData) ->
    StartTime = utils:get_current_millis(),
    ?LOGGER:preciseDebug("[~p]: Handle SYNC EVENT Request(get_status), StateName: ~p~n", [?MODULE, StateName]),
    ?LOGGER:preciseDebug("[~p]: get_status took ~p ~n", [?MODULE, utils:get_current_millis() - StartTime]),
	{reply, [{medium_mode, StateName}, {nodes_to_filter, StateData#state.nodes_to_filter}], StateName, StateData};

handle_sync_event({set_state, NewState}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle set_state, OldState: ~w, NewState: ~w~n", [?MODULE, StateName, NewState]),
	{reply, ok, NewState, StateData};

handle_sync_event({update_nodes_to_filter, NewNodesToFilter}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle update_nodes_to_filter, OldNodesToFilter: ~w, NewNodesToFilter: ~w~n", [?MODULE, StateData#state.nodes_to_filter, NewNodesToFilter]),
	{reply, ok, StateName, StateData#state{nodes_to_filter = NewNodesToFilter}};

handle_sync_event({update_nodes_to_filter, NodesToFilterList}, _From, StateName, StateData) ->
    ?LOGGER:debug("[~p]: Handle update_nodes_to_filter, OldNodesToFilter: ~w, NewNodesToFilter: ~w~n", [?MODULE, StateData#state.nodes_to_filter, NodesToFilterList]),
	{reply, ok, StateName, StateData#state{nodes_to_filter = NodesToFilterList}};

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
preparePayload(Address, Data, SelfAddress)->
    BinaryNextHopAddress = <<Address:?ADDRESS_LENGTH>>,
    BinarySelfAddress = <<SelfAddress:?ADDRESS_LENGTH>>,
    Payload = <<BinarySelfAddress/bitstring, BinaryNextHopAddress/bitstring, Data/bitstring>>,
    if (bit_size(Payload) =< ?MAX_FRAME_LENGTH) ->
            ?LOGGER:preciseDebug("[~p]: prepare_payload Payload: ~p.~n", [?MODULE, Payload]),
            Payload;
        true ->
            ?LOGGER:err("[~p]: prepare_payload Binary Data Length exceeded: byte_size : ~p, bit_size: ~p, ?MAX_FRAME_LENGTH: ~p ~n", [?MODULE, byte_size(Payload), bit_size(Payload), ?MAX_FRAME_LENGTH]),
            {error, "Binary Data Length exceeded"}
    end.

isValidTarget(Target, State)->
    SelfAddress = State#state.self_address,
     case Target of
        SelfAddress ->
            true;
        ?BROADCAST_ADDRESS ->
            true;
        _Else ->
            false
         end.

isValidSource(Source, State)->
    ?LOGGER:debug("[~p]: isValidSource : source:~p , nodes_to_filter: ~p~n", [?MODULE, Source, State#state.nodes_to_filter]),
     not lists:member(Source, State#state.nodes_to_filter).

handle_message(Medium, Source, Target, StateData, Data)->
    case isValidTarget(Target, StateData) of
        true ->
            case isValidSource(Source, StateData) of
                true ->
                    UpperLevelModule = StateData#state.upper_level_module,
                    ?LOGGER:debug("[~p]: handle_message : target and source are valid forwarding to ~p~n", [?MODULE, UpperLevelModule]),
                    UpperLevelModule:handle_incoming_message(StateData#state.upper_level_pid, {Medium, Data});
                _Else ->
                    ?LOGGER:debug("[~p]: handle_message : Source(~w) is NOT valid - IGNORING incoming message ~n", [?MODULE, Source])
            end;
        _Else ->
            ?LOGGER:debug("[~p]: handle_message : target(~w) is NOT valid - IGNORING incoming message ~n", [?MODULE, Target])
    end.
