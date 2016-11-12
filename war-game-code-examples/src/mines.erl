-module(mines).
-behaviour(gen_fsm).
-include("../include/params.hrl").
-export([init/1, findEnemies/2, dead/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/2, stop_fsm/1]).
-define(RADIUS, 15).
-define(SEARCH_TIME, 100).
-define(ANGLE, 360).
-define(DAMAGE, 500).

start_fsm(UnitRecord, LocalServer) -> 
	gen_fsm:start(?MODULE, [UnitRecord, LocalServer], []).

stop_fsm(UnitRef)->
	gen_fsm:send_all_state_event(UnitRef, stop).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {see_range,
				damage,
				location_x,
				location_y,
				team,
				local_server}).

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([UnitRecord, LocalServer]) ->
	%% Starting Mine
	gen_fsm:send_event_after(?SEARCH_TIME, find_enemies_event),
	[X,Y] = UnitRecord#params.location,
	%% Initialize - Send To ETS
	gameManager:insertUnit(LocalServer, self(), #params{type=mines,
															team = 0,
															location = [X,Y], %% [X,Y]
															see_range = {?RADIUS,?ANGLE},
															others = [?DAMAGE],
															state = findEnemies}),
    {ok, findEnemies, #state{see_range = {?RADIUS,?ANGLE},
					  damage = ?DAMAGE,
					  location_x = X,
					  location_y = Y,
					  team = 0,
					  local_server = LocalServer}}.

%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================

%% ============================== Look For Enemies ==============================
findEnemies(find_enemies_event, StateData) ->
	gameManager:findEnemies(StateData#state.local_server, self()),
	gen_fsm:send_event_after(?SEARCH_TIME, find_enemies_event),
    {next_state, findEnemies, StateData};

%% ============================== Enemy Found - Explode Mine ==============================
findEnemies({target_found, ClosestEnemy}, StateData) ->
	{_Distance, EnemyPID, [_Enemy_X, _Enemy_Y]} = ClosestEnemy,
	%% Send Damage
	unit:send_damage(EnemyPID, ?DAMAGE),
	%% Die (mine is not active after sending damage to someone)
	gen_fsm:send_event_after(?DEAD_TIME, delete),
	{next_state, dead, StateData}.

%% ============================== Dead State - Used To Tell Graphics ==============================
dead(find_enemies_event, StateData) -> {next_state, dead, StateData};
dead({target_found, _ClosestEnemy}, StateData) -> {next_state, dead, StateData};
dead(delete, StateData) -> {stop,normal,StateData}.


%% ============================================================================================
%% ===================================== Handle Sync Events ===================================
%% ============================================================================================

%% ============================== Check If Unit Is Alive ==============================
handle_sync_event(is_alive, _From, StateName, StateData) ->
	Reply = self(),
	{reply, Reply, StateName, StateData}.



%% ============================================================================================
%% ======================================== Terminate =========================================
%% ============================================================================================
terminate(_Reason, _StateName, StateData) ->
	gameManager:unitIsDead(StateData#state.local_server, self()),
    ok.




code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.
handle_event(stop, _StateName, StateData) ->
   {stop, normal, StateData};
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


