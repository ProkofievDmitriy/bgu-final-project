-module(resource).
-behaviour(gen_fsm).
-include("../include/params.hrl").
-export([init/1, await/3, dead/2, dead/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/2, stop_fsm/1]).
-define(MAX_VALUE, 200).
-define(LOCAL_SERVER(Record), Record#resource_p.local_server).

start_fsm(UnitRecord, LocalServer) -> 
	gen_fsm:start(?MODULE, [UnitRecord, LocalServer], []).

stop_fsm(UnitRef)->
	gen_fsm:send_all_state_event(UnitRef, stop).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([UnitRecord, LocalServer]) ->
	UnitState = case UnitRecord#params.state of
					%% First Initial
		undefined -> %%Random Type
					Type = case mymath:get_random(120) =< 50 of
				 		true -> iron;
				 		false -> gold
				 	 end,
					%% Random Amount
					 Amount = mymath:get_random(?MAX_VALUE),
					%%Build Record
					 UnitRecord#params{type=resource, 
									  others=#resource_p{type = Type, amount = Amount, local_server=LocalServer},
									  team = 0,
									  state = await};
					%% Save State
		_Else -> UnitRecord#params{others=(UnitRecord#params.others)#resource_p{local_server=LocalServer},
								   state = UnitRecord#params.state}
	end,
	%% Update ETS
	gameManager:insertUnit(LocalServer, self(), UnitState),
    {ok, await, UnitState}.


%% ============================================================================================
%% =========================================== States =========================================
%% ============================================================================================


%% ============= Resource Found - Send Reply with Type and Amount ===============
await(collected, _From, StateData) ->
	#resource_p{type=Type, amount=Amount, local_server=LocalServer} = StateData#params.others,
    Reply = [Type, Amount],	
	gameManager:insertUnit(LocalServer, self(), StateData#params{state=dead}),
	%% Resource "Dead"
	gen_fsm:send_event_after(?DEAD_TIME, delete),
    {reply, Reply, dead, StateData}.


%% ============================== Dead State - Used To Update Graphics ==============================
dead(collected, StateData) -> {next_state, dead, StateData};
dead(delete, StateData) -> {stop,normal,StateData}.
dead(collected, _From, StateData) -> {reply, dead, dead, StateData}.



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


