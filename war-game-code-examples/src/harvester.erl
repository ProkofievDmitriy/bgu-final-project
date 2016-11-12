-module(harvester).
-behaviour(gen_fsm).
-include("../include/params.hrl").
-define(GET_X(List),lists:nth(1, List)).
-define(GET_Y(List),lists:nth(2, List)).
-define(LOCAL_SERVER(Record), Record#harvester_p.local_server).
-define(TARGET_X(Record),Record#harvester_p.target_x).
-define(TARGET_Y(Record),Record#harvester_p.target_y).
-define(TARGET_PID(Record),Record#harvester_p.target_pid).
-define(GOLD(Record),Record#harvester_p.gold).
-define(IRON(Record),Record#harvester_p.iron).
-define(HP(Record),Record#harvester_p.hp).
-define(RADIUS,60).
-define(ANGLE, 360).
-define(STEP_ANGLE, 120).
-define(SMALL_STEP, 3).
-define(LARGE_STEP, 40).
-define(HP,250).

-export([init/1, seek/2, go_to_target/2, dead/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/2, stop_fsm/1]).

start_fsm(UnitRecord, LocalServer) -> 
	gen_fsm:start(?MODULE, [UnitRecord, LocalServer], []).

stop_fsm(UnitRef)->
	gen_fsm:sync_send_all_state_event(UnitRef, stop).
	

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([UnitRecord, LocalServer]) ->
	UnitState = case UnitRecord#params.state of
			%% First Initial
			undefined -> [X,Y] = UnitRecord#params.location,
						 UnitRecord#params{type=harvester, 
										  others=#harvester_p{hp=?HP, local_server=LocalServer, target_x=X, target_y=Y, iron=0, gold=0},
										  direction = mymath:get_random(359),
										  see_range={?RADIUS,?ANGLE},
										  state = seek};
			%% Saving State
			_Else -> UnitRecord#params{others=(UnitRecord#params.others)#harvester_p{local_server=LocalServer},
									   state = UnitRecord#params.state}
	end,
	%% Update ETS
	gameManager:insertUnit(LocalServer, self(), UnitState),
	gen_fsm:send_event_after(?TICK, update_location),
	{ok, UnitState#params.state, UnitState}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% ============================== Seek - Update Location Every TICK ==============================
seek(update_location, StateData) ->
	%% Search for Resources
	gameManager:findEnemies(?LOCAL_SERVER(StateData#params.others), self()),
	%% Calculate Distance to Resource
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
	Params = (StateData#params.others)#harvester_p{target_x=NewTargetX, target_y=NewTargetY, target_pid=undefined},
	NewStateData = StateData#params{location=[NewX,NewY],direction=NewDirection,state=seek,others=Params},
	%% Update Location in Server
	gameManager:updateUnitLocation(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
	gen_fsm:send_event_after(?TICK, update_location),
    {next_state, seek, NewStateData};

%% ============================== Seek - Damage Received ==============================
seek({damage_received, Damage}, StateData) ->
	CurrentHP = ?HP(StateData#params.others),
	NewHP = CurrentHP - Damage,
	Params = (StateData#params.others)#harvester_p{hp=NewHP},
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

%% =========================== Seek - Resource Found - Going To Get It ===========================
seek({target_found, ClosestResource}, StateData) ->
	%% Get Resource PID
	{_Distance, ResourcePID, [ResourceX, ResourceY]} = ClosestResource,
	%% Calculate Direction To Resource
	Direction = case ResourceX < ?GET_X(StateData#params.location) of
					true -> mymath:atan((ResourceY-?GET_Y(StateData#params.location))/(ResourceX-?GET_X(StateData#params.location))) + 180;
					false -> mymath:atan((ResourceY-?GET_Y(StateData#params.location))/(ResourceX-?GET_X(StateData#params.location)))
				end,
	NewDirection = case Direction < 0 of
						true -> round(Direction) + 360;
						false -> round(Direction) rem 360
					end,
	
	Params = (StateData#params.others)#harvester_p{target_pid = ResourcePID, target_x=ResourceX, target_y=ResourceY},
	NewStateData = StateData#params{direction=NewDirection, others=Params, state=go_to_target},
	{next_state, go_to_target, NewStateData}.


%% ============================== Go To Target - Update Location Every TICK ==============================
go_to_target(update_location, StateData) ->
	%% Get Units PID and Location
	Distance_X = ?TARGET_X(StateData#params.others) - ?GET_X(StateData#params.location),
	Distance_Y = ?TARGET_Y(StateData#params.others) - ?GET_Y(StateData#params.location),
	TargetPID = ?TARGET_PID(StateData#params.others),
	%% Check If Target Is Still There (Maybe someone else took it)
	Alive = try unit:is_alive(TargetPID) of
					TargetPID -> TargetPID
	catch
		_Exit:{noproc,_SomeError} -> noproc;
		_Exit:{timeout, _SomeError} -> noproc;
		_Exit:{{nodedown,_Node},_SomeError} -> noproc
	end,
	Result = case Alive of
				 %% Someone took it - Go Back To Seek
				 noproc -> NewStateData = StateData#params{state=seek},
						   gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others),self(), NewStateData),
						   {next_state, seek, NewStateData};
				 %% Keep Going To Target
				 TargetPID -> case ((Distance_X*Distance_X + Distance_Y*Distance_Y) < (?SMALL_STEP*?SMALL_STEP)) of
								  %% Target Reached - Collect Resource
								  true -> 
									  try gen_fsm:sync_send_event(?TARGET_PID(StateData#params.others), collected) of
											  dead -> NewStateData = StateData#params{state=seek},
													  gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others),self(), NewStateData),
													  {next_state, seek, NewStateData};
											  [Type, Amount] -> gameManager:sendSupplyToBase(?LOCAL_SERVER(StateData#params.others),Type,Amount,StateData#params.team),
																%resource:stop_fsm(?TARGET_PID(StateData#params.others)),
																NewStateData = StateData#params{state=seek},
																gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
																{next_state, seek, NewStateData}
										  catch
											  exit:{noproc,_Error} ->  NewStateData = StateData#params{state=seek},
													  					gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others),self(), NewStateData),
													 					 {next_state, seek, NewStateData};
											  exit:{timeout,_Error} ->  NewStateData = StateData#params{state=seek},
													  					gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others),self(), NewStateData),
													 					 {next_state, seek, NewStateData}
										  end;
								  %% Still Not There - Keep Going
								  false -> NewX = ?GET_X(StateData#params.location) + ?SMALL_STEP*mymath:cos(StateData#params.direction),
				 							NewY = ?GET_Y(StateData#params.location) + ?SMALL_STEP*mymath:sin(StateData#params.direction),
											 NewStateData = StateData#params{location=[NewX,NewY], state=go_to_target},
											 gameManager:updateUnitLocation(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
											 {next_state, go_to_target, NewStateData}
							  end
			 end,								  
	%% Timer Event
	gen_fsm:send_event_after(?TICK, update_location),
	Result;

%% ============================== Go To Target - Damage Recieved ==============================
go_to_target({damage_received, Damage}, StateData) ->
	CurrentHP = ?HP(StateData#params.others),
	NewHP = CurrentHP - Damage,
	Params = (StateData#params.others)#harvester_p{hp=NewHP},
	%% Check if CurrentHP - Damage <= 0 - Mark As Killed
	Return = case NewHP =< 0 of
				 %% Dead
		true -> NewStateData = StateData#params{state=dead, others=Params},
				gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				gen_fsm:send_event_after(?DEAD_TIME, delete),
				{next_state,dead,NewStateData};
				 %% Not Dead
		false -> NewStateData = StateData#params{state=go_to_target, others=Params},
				 gameManager:insertUnit(?LOCAL_SERVER(StateData#params.others), self(), NewStateData),
				 {next_state,go_to_target,NewStateData}
	end,
	Return;

go_to_target({target_found, _ClosestResource}, StateData) -> {next_state,go_to_target,StateData}.


%% ============================== Dead State - Used To Tell Graphics ==============================
dead({damage_received, _Damage}, StateData) -> {next_state,dead,StateData};
dead({target_found, _ClosestResource}, StateData) -> {next_state,dead,StateData};
dead(update_location, StateData) -> {next_state,dead,StateData};
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
	NewX = ?GET_X(StateData#params.location) + ?LARGE_STEP*mymath:cos(NewDirection),
	NewY = ?GET_Y(StateData#params.location) + ?LARGE_STEP*mymath:sin(NewDirection),
	%% If New Coordinates Are Out Of Map - Randomize Again
	case NewX > ?GLOBAL_X orelse NewX < 0 orelse NewY > ?GLOBAL_Y orelse NewY < 0 of
		true -> 
				NewStateData = StateData#params{direction = -1*NewDirection + 360},
				random_next_step(NewStateData);
		false -> [NewX,NewY,NewDirection]
	end.
