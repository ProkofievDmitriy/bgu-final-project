-module(tank).
-behaviour(gen_fsm).
-include("../include/params.hrl").
-define(GET_X(List),lists:nth(1, List)).
-define(GET_Y(List),lists:nth(2, List)).
-define(LOCAL_SERVER(Record), Record#tank_p.local_server).
-define(TARGET_X(Record),Record#tank_p.target_x).
-define(TARGET_Y(Record),Record#tank_p.target_y).
-define(TARGET_PID(Record),Record#tank_p.target_pid).
-define(HP(Record),Record#tank_p.hp).
-define(TIMER(Record),Record#tank_p.timer).
-define(RADIUS,140).
-define(ANGLE, 360).
-define(STEP_ANGLE, 120).
-define(SMALL_STEP, 2).
-define(DAMAGE, 70).
-define(LARGE_STEP, 20).
-define(FIRE_RATE, 1500).
-define(HP, 1000).

-export([init/1, seek/2, attack/2, dead/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/2, stop_fsm/1]).

start_fsm(UnitRecord, LocalServer) -> 
	gen_fsm:start(?MODULE, [UnitRecord, LocalServer], []).

stop_fsm(UnitRef)->
	gen_fsm:sync_send_all_state_event(UnitRef, stop).
	

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([UnitRecord, LocalServer]) ->
	UnitState = case UnitRecord#params.state of
			%% First Initial
			undefined -> [X,Y] = UnitRecord#params.location,
						 UnitRecord#params{type=tank, 
										  others=#tank_p{hp=?HP, local_server=LocalServer, target_x=X, target_y=Y},
										  direction = mymath:get_random(359),
										  see_range={?RADIUS,?ANGLE},
										  state = seek};
			%% Saving State
			_Else -> UnitRecord#params{others=(UnitRecord#params.others)#tank_p{local_server=LocalServer},
									   state = UnitRecord#params.state}
	end,
	%% Update ETS
	gameManager:insertUnit(LocalServer, self(), UnitState),
	%% Send Timer Event
	case UnitState#params.state of
		seek -> gen_fsm:send_event_after(?TICK, update_location);
		attack -> gen_fsm:send_event_after(?FIRE_RATE, attack_now);
		dead -> gen_fsm:send_event_after(?DEAD_TIME, delete)
	end,
	{ok, UnitState#params.state, UnitState}.


%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% ============================== Seek - Update Location Every TICK ==============================
seek(update_location, StateData) ->
	%% Search for Enemies
	gameManager:findEnemies(?LOCAL_SERVER(StateData#params.others), self()),
	%% Calculate Distance to Enemy
	Distance_X = ?TARGET_X(StateData#params.others) - ?GET_X(StateData#params.location),
	Distance_Y = ?TARGET_Y(StateData#params.others) - ?GET_Y(StateData#params.location),
	%% CHECK IF REACHED LARGE STEP
	[NewTargetX,NewTargetY,NewDirection] = case ((Distance_X*Distance_X + Distance_Y*Distance_Y) < (?SMALL_STEP*?SMALL_STEP)) of
											   true -> random_next_step(StateData);
											   false -> [?TARGET_X(StateData#params.others),?TARGET_Y(StateData#params.others),StateData#params.direction]
										   end,
	%% SMALL STEP UPDATE
	NewX = ?GET_X(StateData#params.location) + ?SMALL_STEP*mymath:cos(NewDirection),
	NewY = ?GET_Y(StateData#params.location) + ?SMALL_STEP*mymath:sin(NewDirection),
	%% Update location and parameters
	Params = (StateData#params.others)#tank_p{target_x=NewTargetX, target_y=NewTargetY, target_pid=undefined},
	%% New State
	NewStateData = StateData#params{location=[NewX,NewY],direction=NewDirection,state=seek,others=Params},
	%% Update Location in Server
	gameManager:updateUnitLocation(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
	%% Timer Event
	gen_fsm:send_event_after(?TICK, update_location),
    {next_state, seek, NewStateData};


%% ============================== Seek - Damage Received ==============================
seek({damage_received, Damage}, StateData) ->
	CurrentHP = ?HP(StateData#params.others),
	NewHP = CurrentHP - Damage,
	Params = (StateData#params.others)#tank_p{hp=NewHP},
	%% Check if CurrentHP - Damage <= 0 - Mark As Killed
	Return = case NewHP =< 0 of
		true -> NewStateData = StateData#params{state=dead, others=Params},
				gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				gen_fsm:send_event_after(?DEAD_TIME, delete),
				{next_state,dead,NewStateData};
		false -> NewStateData = StateData#params{state=seek, others=Params},
				 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				 {next_state,seek,NewStateData}
	end,
	Return;

%% =========================== Seek - Enemy Found - Going To Attack ===========================
seek({target_found, ClosestTarget}, StateData) ->
	%% Get Target PID
	{_Distance, TargetPID, [TargetX, TargetY]} = ClosestTarget,
	%% Calculate Direction For Shooting
	NewDirection = new_direction(TargetX, TargetY, StateData#params.location),
	%% Update Parameters
	Params = (StateData#params.others)#tank_p{target_pid = TargetPID, target_x=TargetX, target_y=TargetY},
	NewStateData = StateData#params{direction=NewDirection, others=Params, state=attack},
	gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
	gen_fsm:send_event_after(?GO_TO_FIRE, attack_now),
	{next_state, attack, NewStateData}.


%% =========================== Attack - Attack Now! ===========================
attack(update_location, StateData) -> %gen_fsm:send_event_after(?TICK, update_location),
									  {next_state, attack, StateData};
attack({target_found, _ClosestEnemy}, StateData) -> {next_state, attack, StateData};
attack(attack_now, StateData) ->
	%% Get Target PID
	TargetPID = (StateData#params.others)#tank_p.target_pid,
	%% Check If Target Still Alive
	Alive = try unit:is_alive(TargetPID) of
					TargetPID -> TargetPID
	catch
		_Exit:{noproc,_SomeError} -> noproc;
		_Exit:{timeout, _SomeError} -> noproc;
		_Exit:{{nodedown,_Node},_SomeError} -> noproc
	end,
	Reply = case Alive of
				%% No Such Process - Target Is Dead - Go Back To Seek
				noproc -> [NewTargetX,NewTargetY,NewDirection] = random_next_step(StateData),
							 Params = (StateData#params.others)#tank_p{target_pid = undefined, target_x=NewTargetX, target_y=NewTargetY},
							 NewStateData = StateData#params{direction=NewDirection, state=seek, others=Params},
							 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
							 gen_fsm:send_event_after(?TICK, update_location),
							 {next_state, seek, NewStateData};
				%% Attack!
				TargetPID ->
						 %% Send Damage To Target
						 unit:send_damage(TargetPID, ?DAMAGE),
						 %% Update Target Location
						 LocationReply = try unit:get_location(TargetPID) of
													   [X,Y] -> [X,Y]
												   catch
													   	_Exit2:{noproc,_SomeError2} -> noproc;
														_Exit2:{timeout, _SomeError2} -> noproc;
														_Exit2:{{nodedown,_Node2},_SomeError2} -> noproc
													end,
						 case LocationReply of
							 %% Return To Seek
							 noproc -> [NewTargetX,NewTargetY,NewDirection] = random_next_step(StateData),
										 Params = (StateData#params.others)#tank_p{target_pid = undefined, target_x=NewTargetX, target_y=NewTargetY},
										 NewStateData = StateData#params{direction=NewDirection, state=seek, others=Params},
										 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
										 gen_fsm:send_event_after(?TICK, update_location),
										 {next_state, seek, NewStateData};
							 %% Got Target
							[NewTargetX,NewTargetY] -> 
						 DistanceX = NewTargetX-?GET_X(StateData#params.location),
						 DistanceY = NewTargetY-?GET_Y(StateData#params.location),
						 NewDistance = DistanceX*DistanceX + DistanceY*DistanceY,
						 %% Check If Still In Shooting Range
						 case NewDistance =< ?RADIUS*?RADIUS of
							 false -> [TargetX,TargetY,NewDirection] = random_next_step(StateData),
									  Params = (StateData#params.others)#tank_p{target_pid = undefined, target_x=TargetX, target_y=TargetY},
									  NewStateData = StateData#params{direction=NewDirection, state=seek, others=Params},
									  gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
									  gen_fsm:send_event_after(?TICK, update_location),
									  {next_state, seek, NewStateData};
							 true -> NewDirection = new_direction(NewTargetX, NewTargetY, StateData#params.location),
									 Params = (StateData#params.others)#tank_p{target_x=NewTargetX, target_y=NewTargetY},
									 NewStateData = StateData#params{direction=NewDirection, others=Params, state=attack},
									 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
									 gen_fsm:send_event_after(?FIRE_RATE, attack_now),
									 {next_state, attack, NewStateData}
						 end
						 end
	end,
	Reply;

%% =========================== Attack - Damage Received ===========================
attack({damage_received, Damage}, StateData) ->
	CurrentHP = ?HP(StateData#params.others),
	NewHP = CurrentHP - Damage,
	Params = (StateData#params.others)#tank_p{hp=NewHP},
	%% Check if CurrentHP - Damage <= 0 - Mark As Killed
	Return = case NewHP =< 0 of
				 %% Dead
		true -> NewStateData = StateData#params{state=dead, others=Params},
				gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				gen_fsm:send_event_after(?DEAD_TIME, delete),
				{next_state,dead,NewStateData};
				 %%Not Dead
		false -> NewStateData = StateData#params{state=attack, others=Params},
				 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				 {next_state,attack,NewStateData}
	end,
	Return.

%% ============================== Dead State - Used To Tell Graphics ==============================
dead({damage_received, _Damage}, StateData) -> {next_state,dead,StateData};
dead({target_found, _ClosestResource}, StateData) -> {next_state,dead,StateData};
dead(update_location, StateData) -> {next_state,dead,StateData};
dead(attack_now, StateData) -> {next_state,dead,StateData};
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
	{reply, Reply, StateName, StateData};

handle_sync_event(stop, _From, StateName, StateData) ->
	{stop, normal, StateName, StateData}.


%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(_Reason, _StateName, StateData) ->
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

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ============================== Get New Randomize Direction ==============================
random_next_step(StateData) ->
	%% Randomize Angle
	RandomDirection = mymath:get_random(?STEP_ANGLE) - (?STEP_ANGLE div 2),
	%% Calculate Direction
	Direction = StateData#params.direction + RandomDirection,
	NewDirection = case Direction < 0 of
		true -> Direction + 360;
		false -> Direction rem 360
	end,
	%% Calculate New X and New Y
	NewX = ?GET_X(StateData#params.location) + ?LARGE_STEP*mymath:cos(NewDirection),
	NewY = ?GET_Y(StateData#params.location) + ?LARGE_STEP*mymath:sin(NewDirection),
	%% If New Coordinates Are Out Of Map - Randomize Again
	case NewX > ?GLOBAL_X orelse NewX < 0 orelse NewY > ?GLOBAL_Y orelse NewY < 0 of
		true -> 
				NewStateData = StateData#params{direction = -1*NewDirection + 360},
				random_next_step(NewStateData);
		false -> [NewX,NewY,NewDirection]
	end.

%% ============================== Calculate Direction To A Given Target ==============================
new_direction(TargetX,TargetY,MyLocation) ->
	Direction = case TargetX < ?GET_X(MyLocation) of
					true -> mymath:atan((TargetY-?GET_Y(MyLocation))/(TargetX-?GET_X(MyLocation))) + 180;
					false -> mymath:atan((TargetY-?GET_Y(MyLocation))/(TargetX-?GET_X(MyLocation)))
				end,
	case Direction < 0 of
		true -> round(Direction) + 360;
		false -> round(Direction) rem 360
	end.
