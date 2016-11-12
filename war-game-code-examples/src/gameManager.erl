-module(gameManager).
-behaviour(gen_server).
-include("../include/params.hrl").

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(GET_X(List),lists:nth(1, List)).
-define(GET_Y(List),lists:nth(2, List)).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_server/8, start_server/3, killUnit/2, giveETS/2, 
		 updateNodes/2, addBase/3, moveUnitToNode/2, sendSupplyToBase/4, 
		 updateBase/3, updateUnitLocation/3, createNewUnit/3, insertUnit/3, 
		 createMines/2, createResources/2, unitIsDead/2, killAll/1, findEnemies/2, 
		 sendEtsToGraphics/1, stop_server/1]).

-export([get_query_answer/4, monitor_func/3, send_unit_to_node/3]).

start_server(ServerName, X_Start, X_End, Y_Start, Y_End, NodesList, X_Limit, Y_Limit) -> 
	{ok,ServerPID} = gen_server:start({global,ServerName}, ?MODULE, [ServerName, X_Start, X_End, Y_Start, Y_End, NodesList, X_Limit, Y_Limit, undefined], []),
	Parameters = [ServerName, X_Start, X_End, Y_Start, Y_End, NodesList, X_Limit, Y_Limit],
	%% Spawn Monitor
	spawn(gameManager, monitor_func, [ServerPID, ServerName, Parameters]),
	ServerPID.

%%This Case Is For Restarting When Monitor Catched A Crash
start_server(ServerName, Parameters, State) -> 
	NewParams = Parameters ++ [State],
	{ok,ServerPID} = gen_server:start({global,ServerName}, ?MODULE, NewParams, []),
	ServerPID.

giveETS(ServerName, PID) ->
	gen_server:call({global,ServerName}, {giveETS,PID}).

updateNodes(ServerName, Nodes) ->
	gen_server:call({global,ServerName}, {updateNodes, Nodes}).

stop_server(ServerName) ->
	gen_server:stop({global,ServerName}).

killUnit(ServerName, Param) ->
	gen_server:cast({global,ServerName},{killUnit, Param}).

updateUnitLocation(ServerName, UnitPID, UnitRecord) ->
	gen_server:cast({global,ServerName}, {updateUnitLocation, UnitPID, UnitRecord}).

insertUnit(ServerName, UnitPID, NewParameters) ->
	gen_server:cast({global,ServerName}, {insertUnit, UnitPID, NewParameters}).

createNewUnit(ServerName, UnitParameters, 1) ->
	gen_server:cast({global,ServerName}, {createNewUnit, UnitParameters});
createNewUnit(ServerName, UnitParameters, Amount) ->
	gen_server:cast({global,ServerName}, {createNewUnit, UnitParameters}),
	createNewUnit(ServerName, UnitParameters, Amount-1).

moveUnitToNode(ServerName, UnitRecord) ->
	gen_server:cast({global,ServerName}, {moveUnitToNode, UnitRecord}).

createMines(ServerName, NumOfMines) -> 
	gen_server:call({global,ServerName}, {createMines, NumOfMines}).

createResources(ServerName, NumOfResources) -> 
	gen_server:call({global,ServerName},{createResources, NumOfResources}).

unitIsDead(ServerName, UnitPID) -> 
	gen_server:cast({global,ServerName}, {unitIsDead, UnitPID}).

findEnemies(ServerName, UnitPID) ->
	gen_server:cast({global,ServerName}, {findEnemies, UnitPID}).

sendEtsToGraphics(ServerName) ->
	gen_server:cast({global,ServerName}, {toGraphics}).

killAll(ServerName) ->
	gen_server:cast({global,ServerName}, killAll).

addBase(ServerName, UnitPID, NewParameters) ->
	gen_server:cast({global,ServerName}, {addBase, UnitPID, NewParameters}).

updateBase(ServerName, UnitPID, Team) ->
	gen_server:cast({global,ServerName}, {updateBase, UnitPID, Team}).

sendSupplyToBase(ServerName, Type, Amount, Team) ->
	gen_server:cast({global, ServerName}, {sendSupplyToBase, Type, Amount, Team}).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {server_name,
				etsID,
				x_range,
				y_range,
				nodes,
				global_x,
				global_y,
				base_one,
				base_two,
				monitor_pid}).



%% ============================================================================================
%% =========================================== Init ==========================================
%% ============================================================================================
init([ServerName, X_Start, X_End, Y_Start, Y_End, NodesList, X_Limit, Y_Limit, State]) ->
	%% Server is the group leader in this node
	group_leader(whereis(user),self()),
%% 	random:seed(now()),
	%% Create ETS
	Reply = case State of
				undefined -> EtsID = ets:new(localETS, [set, public]),
							 %% Timer For Sending To Graphics
							timer:send_interval(40, updateDataBase),
							io:format("Starting Game Manager...~n"),
							 %% Define State
						    {ok, #state{server_name = ServerName,
										etsID = EtsID,
										x_range = {X_Start, X_End},
										y_range = {Y_Start, Y_End},
										nodes = NodesList,
										global_x = X_Limit,
										global_y = Y_Limit}};
				_Else -> timer:send_interval(40, updateDataBase),
						{ok, State}
			end,
	Reply.
	


%% ============================================================================================
%% ======================================== Handle Call =======================================
%% ============================================================================================

%% ============================== Create Mines ==============================
handle_call({createMines, NumOfMines}, _From, State) ->
	io:format("Creating Mines...~n",[]),
	X_Start = element(1,State#state.x_range),
	X_End = element(2,State#state.x_range),
	Y_Start = element(1,State#state.y_range),
	Y_End = element(2,State#state.y_range),
	%% Generating NumOfMines Random Location
	RandomLocations = [[(X_Start + mymath:get_random(X_End - X_Start)),(Y_Start + mymath:get_random(Y_End - Y_Start))] || _ <- lists:seq(1, NumOfMines)],
	%% Starting Mines FSM
	lists:foreach(fun(Location) -> mines:start_fsm(#params{location=Location},State#state.server_name) end, RandomLocations),
	%% Add To ETS
	Reply = ok,
    {reply, Reply, State};

%% ============================== Create Resources ==============================
handle_call({createResources, NumOfResources}, _From, State) ->
	io:format("Creating Resources...~n",[]),
	X_Start = element(1,State#state.x_range),
	X_End = element(2,State#state.x_range),
	Y_Start = element(1,State#state.y_range),
	Y_End = element(2,State#state.y_range),
	%% Generating NumOfResources Random Location and Amounts
	RandomLocations = [{[(X_Start + mymath:get_random_resource(X_End - X_Start)),(Y_Start + mymath:get_random_resource(Y_End - Y_Start))]} || _ <- lists:seq(1, NumOfResources)],
	%% Starting Resources FSMs
	lists:foreach(fun({Location}) -> resource:start_fsm(#params{location=Location},State#state.server_name) end, RandomLocations),
	Reply = ok,
    {reply, Reply, State};

%% ============================== Give Ets To Monitor ==============================
handle_call({giveETS,PID}, _From, State) ->
	ets:give_away(State#state.etsID, PID, none),
	NewState = State#state{monitor_pid=PID},
	Reply = State,
	{reply, Reply, NewState};

%% ====================== Update Nodes List - Used For Crashing Recovery =======================
handle_call({updateNodes, Nodes},_From,State) ->
	{reply, ok, State#state{nodes=Nodes}}.
	


%% ============================================================================================
%% ======================================== Handle Cast =======================================
%% ============================================================================================


%% ============================== Update Unit Location (NEW) ==============================
handle_cast({updateUnitLocation, UnitPID, UnitRecord}, State) ->
	%% First - Update ETS
	[X,Y] = UnitRecord#params.location,
	ets:insert(State#state.etsID, {UnitPID, UnitRecord}),
	%% Check [X,Y] Node using the nodes List given when initializing
	InNode = in_node(State#state.nodes, [X,Y]),
	%% Check if need to switch nodes
	case InNode =:= State#state.server_name of
		%% Same Node - just update location and direction
		true -> ok;
		%% Different Node - send unit
		false -> send_unit_to_node(InNode, State#state.etsID, UnitPID)
	end,
	{noreply, State};


%% ============================== Create New Unit ==============================
handle_cast({createNewUnit, [Type,Team,Location]}, State) ->
	%% Build Params Record
	Params = #params{type=Type,location=Location,team=Team},
	% Start Unit FSM
	Type:start_fsm(Params, State#state.server_name),
    {noreply, State};



%% ============================== Create New Unit ==============================
handle_cast({moveUnitToNode, Params}, State) ->
	%% Start Unit FSM
	UnitType = Params#params.type,
	%% Start Unit FSM
	UnitType:start_fsm(Params, State#state.server_name),
    {noreply, State};



%% ============================ Inserting Unit To ETS ==============================
handle_cast({insertUnit, UnitPID, NewParameters}, State) ->
	ets:insert(State#state.etsID,{UnitPID, NewParameters}),
	{noreply, State};

%% ============================ Adding Base Reference To All Nodes ==============================
handle_cast({addBase, UnitPID, NewParameters}, State) ->
	ets:insert(State#state.etsID,{UnitPID, NewParameters}),
	lists:foreach(fun({Node,_X,_Y}) -> gameManager:updateBase(Node, UnitPID, NewParameters#params.team) end, State#state.nodes),
	{noreply, State};

%% ============================ Updating Base Reference ==============================
handle_cast({updateBase, UnitPID, Team}, State) ->
	NewState = case Team of
		1 -> State#state.monitor_pid ! {base_one, UnitPID},
			 State#state{base_one=UnitPID};
		2 -> State#state.monitor_pid ! {base_two, UnitPID},
			 State#state{base_two=UnitPID}
			   end,
	{noreply, NewState};

%% ============================ Supply [Type,Amount] Arrived to a Team ==============================
handle_cast({sendSupplyToBase, Type, Amount, Team}, State) ->
	case Team of
		1 -> try base:supply(State#state.base_one, Type, Amount) of
				 _A -> ok
			 catch
				 _Something:{badarg,_Error} -> ok;
				 _Something:{noproc,_Error} -> ok
			 end;
		2 -> try base:supply(State#state.base_two, Type, Amount) of
				 _A -> ok
			 catch
				 _Something:{badarg,_Error} -> ok;
				 _Something:{noproc,_Error} -> ok
			 end
  	end,
	{noreply, State};

%% ============================ Find Enemies Around Unit ==============================
handle_cast({findEnemies, UnitPID}, State) ->
	
	case ets:lookup(State#state.etsID, UnitPID) of
		[{_PID,UnitRecord}] -> 
	%% Getting Unit Parameters
	[X,Y] = UnitRecord#params.location, 
	{R, _Angle} = UnitRecord#params.see_range,
	Team = UnitRecord#params.team,
	Type = UnitRecord#params.type,
	%% What Nodes To Search
	Right = in_node(State#state.nodes, [X+R,Y]),
	Left = in_node(State#state.nodes, [X-R,Y]),
	Bottom = in_node(State#state.nodes, [X,Y+R]),
	Top = in_node(State#state.nodes, [X,Y-R]),
	%% Build Query - Depending on Unit Type
	MS = case Type of
		harvester -> ets:fun2ms(fun({PID,#params{type=UnitType,
											 team=UnitTeam,
											 location=[UnitX,UnitY]
											 }}) when UnitTeam =:= 0, UnitType=:=resource,
													((UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y) < R*R) -> {(UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y),PID,[UnitX,UnitY]} end);
		soldier -> ets:fun2ms(fun({PID,#params{type=EnemyType,
											 team=EnemyTeam,
											 location=[UnitX,UnitY]
											 }}) when EnemyTeam =/= Team, EnemyTeam =/= 0, 
													((EnemyType=:=soldier) or (EnemyType=:=harvester) or (EnemyType=:=base)),
													((UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y) < R*R) -> {(UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y),PID,[UnitX,UnitY]} end);
		mines -> ets:fun2ms(fun({PID,#params{type=EnemyType,
											 team=EnemyTeam,
											 location=[UnitX,UnitY]
											 }}) when EnemyTeam =/= Team,
													((EnemyType=:=soldier) or (EnemyType=:=tank) or (EnemyType=:=harvester)),
													((UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y) < R*R) -> {(UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y),PID,[UnitX,UnitY]} end);
		tank -> ets:fun2ms(fun({PID,#params{type=EnemyType,
											 team=EnemyTeam,
											 location=[UnitX,UnitY]
											 }}) when EnemyTeam =/= Team, EnemyTeam =/= 0, 
													((EnemyType=:=soldier) or (EnemyType=:=tank) or (EnemyType=:=harvester) or (EnemyType=:=base)),
													((UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y) < R*R) -> {(UnitX-X)*(UnitX-X) + (UnitY-Y)*(UnitY-Y),PID,[UnitX,UnitY]} end)
		end,
	%% Returns a list of nodes to send the query to, if empty - only me
	ListOfNodes = send_query_to_nodes([Right,Left,Bottom,Top],State#state.server_name, []),	
	%% Function to send the query to other nodes and wait for response (spawn)
	spawn(gameManager,get_query_answer, [ListOfNodes,UnitPID, MS, State#state.etsID]);
	[] -> ok
	end,
	{noreply, State};

%% ======================== Do Query on My ETS And Deliver To Spawned Process ===========================
handle_cast({answerQuery, From, MS}, State) -> 
	Answer = query_on_ets(State#state.etsID,MS),
	From ! Answer,
	{noreply, State};

%% ======================== Unit Is Dead - Erase From ETS ===========================
handle_cast({unitIsDead, UnitPID}, State) ->
	%% Delete Unit From Local ETS
	ets:delete(State#state.etsID, UnitPID),
	{noreply, State};

%% ======================== Kill All Units Function ===========================
handle_cast(killAll, State) ->
	EtsAsList = ets:tab2list(State#state.etsID),
	io:format("~p~n",[EtsAsList]),
	lists:foreach(fun({UnitPID,#params{type=Type}}) ->
						  try Type:stop_fsm(UnitPID) of
							  _A -> ok
						  catch
							  _Exit:{noproc, _Error} -> ok;
							  _Exit:{timeout, _Error} -> ok
						  end 
				  end, EtsAsList),
	ets:delete_all_objects(State#state.etsID),
	{noreply, State};

%% ======================== Used To Kill Units When Recovering Server After Crash ===========================
handle_cast({killUnit,[XStart,XEnd,YStart,YEnd]}, State) ->
	%% Query ETS To Find The Right Units
	MS = ets:fun2ms(fun({PID,#params{type=Type,
									location=[UnitX,UnitY]
									}}) when (XStart =< UnitX),(XEnd > UnitX),(YStart =< UnitY),(YEnd > UnitY) ->
														{PID, Type} end),
	%% Delete Units From ETS
	ListToDelete = query_on_ets(State#state.etsID,MS),
	%% Stop FSMs
	lists:foreach(fun({PID,Type}) -> try Type:stop_fsm(PID) of
										  _A -> ok
									  catch
										  _Error:{noproc, _SomeError} -> ok;
									  	  _Error:{timeout,_SomeError} -> ok
									  end, ets:delete(State#state.etsID, PID) end, ListToDelete),
	{noreply, State}.
	

%% ============================================================================================
%% ======================================== Handle Info =======================================
%% ============================================================================================

%% ======================== Send ETS To Graphics ===========================
handle_info(updateDataBase, State) ->
	ETS_List = ets:tab2list(State#state.etsID),
	g:updateDataBase(ETS_List, State#state.server_name),
	{noreply, State}.


%% ============================================================================================
%% ========================================= Terminate ========================================
%% ============================================================================================
terminate(Reason, State) ->
	case Reason of
		normal -> 	EtsAsList = ets:tab2list(State#state.etsID),
					io:format("~p~n",[EtsAsList]),
					lists:foreach(fun({Ref,#params{type=Type}}) ->
					  				try Type:stop_fsm(Ref) of
						  				_A -> ok
					  				catch
						  				_Exit:{noproc, _Error} -> ok;
										_Exit:{timeout, _Error} -> ok
					  				end 
			  					end, EtsAsList);
		_Else -> ok
	end,
    ok.


code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% =================== Send Unit To Different Node ====================
send_unit_to_node(Where, ETS, UnitPID) ->
	case ets:lookup(ETS, UnitPID) of
		[{_PID,UnitRecord}] -> 
			%% Get Unit Type
			Type = UnitRecord#params.type,
			%% Stop Unit In My Node
			try Type:stop_fsm(UnitPID) of
				  _A -> 
					  gameManager:moveUnitToNode(Where, UnitRecord)
			  catch
				  _Exit:{noproc, _Error} -> ok
			  end;
			%% Send Unit To Other Node
		[] -> ok
	end.


%% =================== Return Node Name Of [X,Y] ====================
in_node([],[_X,_Y]) -> undefined;
in_node([{NodeName, [XStart,XEnd],[YStart,YEnd]}|ListOfRanges], [X,Y]) ->
	if 
		(XStart =< X) and (XEnd > X) and (YStart =< Y) and (YEnd > Y) ->
			NodeName;
		true -> in_node(ListOfRanges, [X,Y])
	end.

%% ====================  Returns A List Of All Nodes To Send =====================
send_query_to_nodes([],_MyName,NodesToSend) -> NodesToSend;
send_query_to_nodes([H|PossibleNodes],MyName,NodesToSend) ->
	case H of
		MyName -> send_query_to_nodes(PossibleNodes,MyName,NodesToSend);
		undefined -> send_query_to_nodes(PossibleNodes,MyName,NodesToSend);
		Node -> send_query_to_nodes(PossibleNodes,MyName,[Node|NodesToSend])
	end.

%% =================== This Will Run In Spawn - Send Query To All Nodes ====================
get_query_answer(ListOfNodes, UnitPID, MS, ETS) ->
	lists:foreach(fun(Node) -> gen_server:cast({global,Node}, {answerQuery, self(), MS}) end,ListOfNodes),
	Targets = wait_for_all_messages(length(ListOfNodes),[]) ++ query_on_ets(ETS,MS),
	%% Check If Unit IS Alive
	Alive = try unit:is_alive(UnitPID) of
		UnitPID -> UnitPID
	catch
		exit:{noproc,_SomeError} -> noproc;
		exit:{timeout, _SomeError} -> noproc
	end,
	%% Get Closest Target And Sent Target Found Event To Unit
	case Targets of
		[] -> none;
		_ListOfEnemies -> ClosestTarget = lists:min(Targets),
						  case Alive of
							  noproc -> ok;
							  UnitPID -> unit:target_found(UnitPID, ClosestTarget)
						  end
	end,
	ok.

%% =================== Query MS on ETS ====================
query_on_ets(ETS,MS) -> qlc:eval(ets:table(ETS, [{traverse, {select, MS}}])).

%% =================== Wait For Response From All Nodes ====================
wait_for_all_messages(0,Message) -> Message;
wait_for_all_messages(N,Message) ->
	receive
		Msg -> wait_for_all_messages(N-1,Msg++Message)
	after 3000 ->
		wait_for_all_messages(N-1,Message) %% if some node is not returning message
	end.
	


%% =================== Monitor Functions ====================
monitor_func(PID, ServerName, Parameters) ->
	%% Let The Monitor Be The Owner Of the ETS
	State = gameManager:giveETS(ServerName, self()),
	%% Start Monitor
	erlang:monitor(process, PID),
	start_monitoring(PID, ServerName, Parameters, State).
	
start_monitoring(PID, ServerName, Parameters, State) ->	
	receive
		%% Server Is Down - Recovering...
		{'DOWN', _MonitorRef, _Type, _Object, _Info} -> NewPID = gameManager:start_server(ServerName, Parameters, State),
														erlang:monitor(process, NewPID),
														start_monitoring(NewPID, ServerName, Parameters, State);
		%% Set Bases PIDs For Recovering If Server Crashes
		{base_one, UnitPID} -> start_monitoring(PID, ServerName, Parameters, State#state{base_one=UnitPID});
		{base_two, UnitPID} -> start_monitoring(PID, ServerName, Parameters, State#state{base_two=UnitPID})
	end.

	

