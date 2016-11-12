-module(unit).
-export([get_location/1, send_damage/2, target_found/2, is_alive/1]).

%% Unit API - Used To Send Event To Unit From Server / Different Unit

%% Get Location Of Unit (SYNC)
get_location(UnitRef) ->
	gen_fsm:sync_send_all_state_event(UnitRef, get_location, 10000).

%% Send Damage To Unit (ASYNC)
send_damage(UnitRef, Damage) ->
	gen_fsm:send_event(UnitRef, {damage_received, Damage}).

%% Target Found Event (ASYNC)
target_found(UnitRef, Target) -> 
	gen_fsm:send_event(UnitRef, {target_found,Target}).

%% Ask If Unit Is Alive (SYNC)
is_alive(UnitRef) ->
	gen_fsm:sync_send_all_state_event(UnitRef, is_alive).
	