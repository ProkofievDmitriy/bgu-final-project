-module(base).
-behaviour(gen_fsm).
-include("../include/params.hrl").
-define(GET_X(List),lists:nth(1, List)).
-define(GET_Y(List),lists:nth(2, List)).
-define(LOCAL_SERVER(Record), Record#base_p.local_server).
-define(GOLD(Record),Record#base_p.gold).
-define(IRON(Record),Record#base_p.iron).
-define(HARV(Record),Record#base_p.harv_num).
-define(SOLDIER_COST, 50).
-define(TANK_COST, 80).
-define(HP(Record),Record#base_p.hp).
-define(CREATE_UNIT_TICK, 10000).
-define(MAX_HARV, 15).
-define(SPAWN_LOCATION, 20).
-define(HP,20000).

-export([init/1, await/2, dead/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/2, stop_fsm/1, supply/3]).

start_fsm(UnitRecord, LocalServer) -> 
	gen_fsm:start(?MODULE, [UnitRecord, LocalServer], []).

stop_fsm(UnitRef)->
	gen_fsm:send_all_state_event(UnitRef, stop).
	
supply(UnitRef, Type, Amount) ->
	gen_fsm:send_event(UnitRef, {supply, Type, Amount}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================


%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([UnitRecord, LocalServer]) ->
	UnitState = case UnitRecord#params.state of
					%% First Initial
			undefined -> UnitRecord#params{type=base, 
										  others=#base_p{hp=?HP, local_server=LocalServer,iron=0, gold=0, harv_num=0},
										  state = await};
					%% Save State
			_Else -> UnitRecord#params{others=(UnitRecord#params.others)#base_p{local_server=LocalServer},
									   state = UnitRecord#params.state}
	end,
	%% Add Base To Server - Will Cause Updating Base Referance At All Servers
	gameManager:addBase(LocalServer, self(), UnitState),
	gen_fsm:send_event_after(?CREATE_UNIT_TICK, create_units),
	{ok, UnitState#params.state, UnitState}.



%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% ============================== Await - Create Unit  ==============================
await(create_units, StateData) ->
	%% Get Spawn Location For New Unit
	X = case ?GET_X(StateData#params.location) < (?GLOBAL_X / 2) of
						 true  -> (?GET_X(StateData#params.location) + ?SPAWN_LOCATION);
						 false -> (?GET_X(StateData#params.location) - ?SPAWN_LOCATION)
		end,
	Spawn_Location = [X,?GET_Y(StateData#params.location)],
	%% Create new Harvester Until Maximum Number
	NumOfHarvesters = case ?HARV(StateData#params.others) < ?MAX_HARV of
							false -> ?HARV(StateData#params.others);
							true ->
								gameManager:createNewUnit(?LOCAL_SERVER(StateData#params.others), [harvester,StateData#params.team,Spawn_Location], 1),
								?HARV(StateData#params.others)+1
					end,
	%% Create new Soldier If Enough GOLD
	NewGold = case ?GOLD(StateData#params.others) >= ?SOLDIER_COST of
				  false -> ?GOLD(StateData#params.others);
				  true ->
					  gameManager:createNewUnit(?LOCAL_SERVER(StateData#params.others), [soldier,StateData#params.team,Spawn_Location], 1),
					  (?GOLD(StateData#params.others) - ?SOLDIER_COST)
			  end,
	%% Create New Tank If Enough Iron
	NewIron = case ?IRON(StateData#params.others) >= ?TANK_COST of
				  false -> ?IRON(StateData#params.others);
				  true ->
					  gameManager:createNewUnit(?LOCAL_SERVER(StateData#params.others), [tank,StateData#params.team,Spawn_Location], 1),
					  (?IRON(StateData#params.others) - ?TANK_COST)
			  end,
	%% Update parameters and State
	Params = (StateData#params.others)#base_p{iron=NewIron, gold=NewGold, harv_num=NumOfHarvesters},
	NewStateData = StateData#params{state=await,others=Params},
	gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
	gen_fsm:send_event_after(?CREATE_UNIT_TICK, create_units),
    {next_state, await, NewStateData};


%% ============================== Await - Damage Receieved ==============================
await({damage_received, Damage}, StateData) ->
	CurrentHP = ?HP(StateData#params.others),
	NewHP = CurrentHP - Damage,
	Params = (StateData#params.others)#base_p{hp=NewHP},
	%% Check if CurrentHP - Damage <= 0 - Mark As Killed
	Return = case NewHP =< 0 of
		%% Kill Unit
		true -> NewStateData = StateData#params{state=dead, others=Params},
				gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				gen_fsm:send_event_after(?DEAD_TIME, delete),
				{next_state,dead,NewStateData};
		%% Update HP
		false -> NewStateData = StateData#params{state=await, others=Params},
				 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				 {next_state,await,NewStateData}
	end,
	Return;


%% ============================== Await - Supply Arrived ==============================
await({supply, Type, Amount}, StateData) ->
	Params = case Type of
				 %% Add Iron
				 iron ->
					 CurrentAmount = (StateData#params.others)#base_p.iron,
					 NewAmount = CurrentAmount + Amount,
					 (StateData#params.others)#base_p{iron=NewAmount};
				 %% Add Gold
				 gold ->
					 CurrentAmount = (StateData#params.others)#base_p.gold,
					 NewAmount = CurrentAmount + Amount,
					 (StateData#params.others)#base_p{gold=NewAmount}
			 end,
	%% Update State
	NewStateData = StateData#params{others=Params, state=await},
	gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
	{next_state, await, NewStateData}.

%% ============================== Dead State - Used To Tell Graphics ==============================
dead({damage_received, _Damage}, StateData) -> {next_state,dead,StateData};
dead({supply, _Type, _Amount}, StateData) -> {next_state,dead,StateData};
dead(create_units, StateData) -> {next_state,dead,StateData};
dead(delete, StateData) -> {stop,normal,StateData}.



%% ============================================================================================
%% ===================================== Handle Sync Events ===================================
%% ============================================================================================

%% ============================== Get Location Of Unit ==============================
handle_sync_event(get_location, _From, StateName, StateData) ->
    Reply = [?GET_X(StateData#params.location),?GET_Y(StateData#params.location)],
    {reply, Reply, StateName, StateData};

%% ============================== Check If Unit Is Alive ==============================
handle_sync_event(is_alive, _From, StateName, StateData) ->
	Reply = self(),
	{reply, Reply, StateName, StateData}.



%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(_Reason, _StateName, StateData) ->
	%% Tell Server To Erase From ETS
	gameManager:unitIsDead(?LOCAL_SERVER(StateData#params.others), self()),
    ok.



code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.
handle_event(stop, _StateName, StateData) ->
   {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

