-module(g).
-author('Dmitriy Prokofiev').
-behaviour(gen_server).

-include("../include/params.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([test_network/1, start/0,  start/1 ,stop/0, kill_all/0, game_start/0, updateDataBase/2, insert_debug_unit/1, spawn_new_unit/2]).

-export([ets_each/2, ping_node/1, updateNodes_delayed/4]).

-record(game_state, {database, list_of_nodes, nodes_addresses, timer}).


% test network connection with nodes
test_network([ListOfNodes|_])->
	{ok, Ts, _} = erl_scan:string(ListOfNodes),
	io:fwrite("~n~n~n", []),
	Func = fun(X) -> 
				   case X of
					   {atom, _ , Address} -> 
						   case net_adm:ping(Address) of
						  	 pong-> io:fwrite("Node ~p is UP ~n", [Address]);
						  	 pang-> io:fwrite("Can't reach node ~p~n", [Address]);
							 Other-> io:fwrite("ERROR :  ~p~n", [Other])
					   	 	end;
					   _Else -> ok				
				   end
				end,
	[Func(X) || X <- Ts],
	?DEBUG("nodes() : ~p~n", [nodes()]),
	?DEBUG("active nodes names : ~p~n", [[get_node_name(X) || X<-nodes()]]),
	io:fwrite("~n~n~n", []).



% These are all wrappers for calls to the server
start() -> 
	gen_server:start_link({global, ?DATA_SERVER}, ?MODULE, [], []).

start(ListOfNodes) -> 
	gen_server:start_link({global, ?DATA_SERVER}, ?MODULE, [ListOfNodes], []).

stop() -> 
		gen_server:stop({global,?DATA_SERVER}),
		stop_game(),
		t:stop().

updateDataBase(Data, State) 		-> 	gen_server:cast({global, ?DATA_SERVER}, {updateDataBase, Data, State}).
game_start()						-> 	gen_server:call({global, ?DATA_SERVER}, {start_game}).
spawn_new_unit(UnitParams, Amount)	->	gen_server:cast({global, ?DATA_SERVER}, {spawn_new_unit, UnitParams, Amount}).
insert_debug_unit(Data) 			-> 	gen_server:cast({global, ?DATA_SERVER}, {insert_debug_unit, Data}).
kill_all()							->  gen_server:cast({global, ?DATA_SERVER}, {kill_all}).


% This is called when a connection is made to the server
init(Params) ->
	case Params of
		[ListOfNode|[]] -> test_network(ListOfNode);
		_ -> ok
	end,

	global:register_name(?DATA_SERVER, self()),
	group_leader(whereis(user),self()),
	DataBase = ets:new(?DBNAME, [set, public, named_table]),
	ets:new(?LOCAL_STORAGE, [set, public, named_table]),
	ets:insert(?LOCAL_STORAGE, {{?RED_TEAM, ?GRAPHICS_SERVER}, create_empty_team_stat() }),
	ets:insert(?LOCAL_STORAGE, {{?BLUE_TEAM, ?GRAPHICS_SERVER}, create_empty_team_stat() }),
	setUp(),
	{ok, #game_state{database=DataBase, list_of_nodes=?LIST_OF_NODES, nodes_addresses=nodes()}}.

% handle_call is invoked in response to gen_server:call
handle_call({start_game}, _From, State=#game_state{list_of_nodes=CurrentListOfNodes}) -> 
	start_game_to_nodes(CurrentListOfNodes),
	{reply, none ,State}.

% Asynchronious Calls
handle_cast({kill_all}, State=#game_state{database=DataBase, list_of_nodes=ListOfNodes, timer=Timer}) -> 
	?DEBUG("received kill all ~n", []),
	timer:cancel(Timer),
	lists:foreach(fun({ServerName, _ , _}) -> gameManager:killAll(ServerName) end, ListOfNodes),
	lists:foreach(fun({Key,_UnitRecord}) -> ets:delete(DataBase, Key) end, ets:tab2list(DataBase)),
	ets:insert(?LOCAL_STORAGE, {{?RED_TEAM, ?GRAPHICS_SERVER}, create_empty_team_stat() }),
	ets:insert(?LOCAL_STORAGE, {{?BLUE_TEAM, ?GRAPHICS_SERVER}, create_empty_team_stat() }),
	{noreply, State};

%% Spawn New Unit On Mouse Click
handle_cast({spawn_new_unit, [Type, Team, [X,Y]], Amount}, State=#game_state{list_of_nodes=ListOfNodes})->
	?DEBUG("Spaw new unit Location = ~p , In node = ~p~n",[[X,Y],in_node(ListOfNodes, [X,Y])]),
			gameManager:createNewUnit(in_node(ListOfNodes, [X,Y]), [Type, Team, [X,Y]], Amount),
			{noreply, State};


handle_cast({insert_debug_unit, {Key, Value}}, State=#game_state{database=DataBase})->
				ets:insert(DataBase, {Key, Value}),
	{noreply, State};

%% Update DataBase - Happens Every 40ms - Nodes Sends ETS
handle_cast({updateDataBase, ReceivedEtsList, Server}, State=#game_state{database=DataBase})->
	MS = ets:fun2ms(fun({{PID,NodeName},_Value}) when NodeName=:=Server -> {PID,NodeName} end),
	ListToUpdate = qlc:eval(ets:table(DataBase, [{traverse, {select, MS}}])),
	%% Delete All Units
	ListToDelete = lists:foldl(fun({PID, Value}, List) -> 
						ets:insert(DataBase, {{PID,Server}, Value#params{img=get_old_img_num({PID, Server}, Value#params.state),
																		 frame_delay=update_frame_delay({PID, Server}, Value#params.state)}}),
						case lists:keyfind(PID, 1, List) of
							  false ->  % no such unit
								  List;				 
							  {PID, Server} -> 
								  lists:keydelete(PID , 1, List)
						end 
						end, ListToUpdate, ReceivedEtsList),
	lists:foreach(fun(Key) -> ets:delete(DataBase, Key) end, ListToDelete),
 	update_teams_stat(DataBase),
	{noreply, State};


% We get compile warnings from gen_server unless we define these
handle_cast(error, S) ->
    io:format("StackTrace: ~p~n", S),
    {noreply, S};

handle_cast(_Message, State) -> {noreply, State}.


handle_info({nodedown, Atom}, State=#game_state{database=DataBase, list_of_nodes=CurrentNodesList}) ->
	
	?DEBUG("NODE DOWN RECEIVED : ~p ~p ~n", [Atom, CurrentNodesList]),
	
	NodeDown = get_node_name(Atom), %% Get Node Name As Atom
	NewNodesList = change_nodes_list(NodeDown, CurrentNodesList, down),
	NewNode = replace_node(NodeDown),
	%% Update Nodes List At All Current Servers
	lists:foreach(fun(ServerName) when ServerName=/=NodeDown ->
						  gameManager:updateNodes(ServerName, NewNodesList);
					 (_Else) -> ok end, get_active_nodes_list(NewNodesList)),
	%% Query ETS To Get All Units That Were Inside the Node That Fell
	MS = ets:fun2ms(fun({{PID, NodeName}, Value}) when NodeName=:=NodeDown -> {{PID, NodeName}, Value} end),
	UnitsToMove = qlc:eval(ets:table(DataBase, [{traverse, {select, MS}}])),
	%% Erase All Old Units From Ets
	lists:foreach(fun({Key,_UnitRecord}) -> ets:delete(DataBase, Key) end, UnitsToMove),
	%% Create New Ones In The New Server
	lists:foreach(fun({_Key,UnitRecord}) -> gameManager:moveUnitToNode(NewNode, UnitRecord) end, UnitsToMove),
	spawn(g, ping_node, [Atom]), %% Start Checking If Node is Up Again
	%% Erase All Old Units From Ets
	lists:foreach(fun({Key,_UnitRecord}) -> ets:delete(DataBase, Key) end, UnitsToMove),
	{noreply, State#game_state{list_of_nodes=NewNodesList}};

handle_info({nodeup, Atom}, State=#game_state{database=DataBase, list_of_nodes=CurrentNodeList}) ->
				
	?DEBUG("NODE UP RECEIVED : ~p ~p ~n", [Atom, CurrentNodeList]),
	%% Get Node Name As Atom
	NodeUp = get_node_name(Atom),
	OldNode = replace_node(NodeUp),
	%% Start Node
	[XStart,XEnd,YStart,YEnd] = node_space(NodeUp),
	NewListofNodes = change_nodes_list(NodeUp, CurrentNodeList, up),
	rpc:call(Atom, gameManager, start_server, [NodeUp] ++ node_space(NodeUp) ++ [NewListofNodes, ?WORLD_WIDTH, ?WORLD_HEIGHT]),
	%% Set Query
	MS = ets:fun2ms(fun({{PID,NodeName},#params{type=Type,
														team=Team,
														img=Img,
														location=[UnitX,UnitY],
														direction=Direction,
														see_range=SeeRange,
														others=Others,
														state=UnitState}}) when (XStart =< UnitX),(XEnd > UnitX),(YStart =< UnitY),(YEnd > UnitY),
																			((Type==base) or (Type==mines) or (Type==resource) or (Type==tank) or (Type==soldier) or (Type==harvester)) ->
									{{PID,NodeName},#params{type=Type,
															team=Team,
															img=Img,
															location=[UnitX,UnitY],
															direction=Direction,
															see_range=SeeRange,
															others=Others,
															state=UnitState}} end),
			MS2 = ets:fun2ms(fun({{PID,_Node},#params{type=Type,
														team=Team}}) when Type=:=base -> {PID,Team} end),
			Bases = qlc:eval(ets:table(DataBase, [{traverse, {select, MS2}}])),
			UnitsToMove = qlc:eval(ets:table(DataBase, [{traverse, {select, MS}}])),
			%% Kill All Units From Old Server
			gameManager:killUnit(OldNode, [XStart,XEnd,YStart,YEnd]),
			%% Create New Ones In The New Server
			spawn(g,updateNodes_delayed,[NodeUp, UnitsToMove, NewListofNodes, Bases]),
	{noreply, State#game_state{list_of_nodes=NewListofNodes}}.

terminate(_Reason, State) -> {stop, normal, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       INTERNAL FUNCTIONS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
create_empty_team_stat() ->  #team_stat{gold = 0, iron = 0, harvesters = 0 , soldiers = 0 , tanks = 0, units_killed = 0 }.

%% Update Statistics
update_teams_stat(DataBase)->
	
	%% Build All Queries For Statistics
	BlueBaseQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team , others=Others}}) when (Type==base), (Team==?BLUE_TEAM) -> Others end),	
	RedBaseQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team , others=Others}}) when (Type==base), (Team==?RED_TEAM) -> Others end),	
	BlueTankQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team}}) when (Type==tank), (Team==?BLUE_TEAM) -> 1 end),	
	RedTankQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team}}) when (Type==tank), (Team==?RED_TEAM) -> 1 end),	
	BlueSoldierQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team }}) when (Type==soldier), (Team==?BLUE_TEAM) -> 1 end),	
	RedSoldierQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team }}) when (Type==soldier), (Team==?RED_TEAM) -> 1 end),	
	BlueHarvesterQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team }}) when (Type==harvester), (Team==?BLUE_TEAM) -> 1 end),	
	RedHarvesterQuery = ets:fun2ms(fun({{PID,NodeName},#params{type=Type, team=Team }}) when (Type==harvester), (Team==?RED_TEAM) -> 1 end),	

	%% Update Statistics Parameters
	[#base_p{iron=BlueIron, gold=BlueGold}] = case qlc:eval(ets:table(DataBase, [{traverse, {select, BlueBaseQuery}}])) of
												  [] -> [#base_p{iron=0, gold=0}];
												  [#base_p{iron=Iron, gold=Gold} | _Tail] -> [#base_p{iron=Iron, gold=Gold}]
											  end,
	[#base_p{iron=RedIron, gold=RedGold}] = case qlc:eval(ets:table(DataBase, [{traverse, {select, RedBaseQuery}}])) of
												  [] -> [#base_p{iron=0, gold=0}];
												  [#base_p{iron=Iron2, gold=Gold2} | _Tail2] -> [#base_p{iron=Iron2, gold=Gold2}]
											end,
	BlueSoldiers = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, BlueSoldierQuery}}]))),
	RedSoldiers = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, RedSoldierQuery}}]))),
	BlueTanks = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, BlueTankQuery}}]))),
	RedTanks = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, RedTankQuery}}]))),
	BlueHarvesters = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, BlueHarvesterQuery}}]))),
	RedHarvesters = lists:foldl(fun(X,Acc) -> Acc + X end , 0, qlc:eval(ets:table(DataBase, [{traverse, {select, RedHarvesterQuery}}]))),
	
	[{{?BLUE_TEAM, ?GRAPHICS_SERVER} ,BlueTeamStat}] = ets:lookup(?LOCAL_STORAGE, {?BLUE_TEAM, ?GRAPHICS_SERVER}),
	[{{?RED_TEAM, ?GRAPHICS_SERVER}, RedTeamStat}] = ets:lookup(?LOCAL_STORAGE, {?RED_TEAM, ?GRAPHICS_SERVER}),
	
	%% Update ETS
	ets:insert(?LOCAL_STORAGE, {{?RED_TEAM, ?GRAPHICS_SERVER}, RedTeamStat#team_stat{iron=RedIron, gold=RedGold, tanks=RedTanks, harvesters=RedHarvesters, soldiers=RedSoldiers}}),
	ets:insert(?LOCAL_STORAGE, {{?BLUE_TEAM, ?GRAPHICS_SERVER}, BlueTeamStat#team_stat{iron=BlueIron, gold=BlueGold, tanks=BlueTanks, harvesters=BlueHarvesters, soldiers=BlueSoldiers}}),
	
	ok.

%Update img num on newly received unit
get_old_img_num(Key, State)->
	case ets:lookup(?DBNAME, Key) of
		[] -> 1;
		[{{_Pid,_Node}, Params}] -> 
			case Params#params.state =:= State of
				true -> Params#params.img;
				false -> 
					1
			end
	end.

%Update frame delay on new received unit
update_frame_delay(Key, State)->
	case ets:lookup(?DBNAME, Key) of
		[] -> ?FRAME_DELAY;
		[{{_Pid,_Node}, Params}] -> 
			case Params#params.state =:= State of
				true -> Params#params.frame_delay;
				false -> 
					?FRAME_DELAY
			end
	end.

%Hepl function to traverse ets
ets_each(TableRef, Fun) ->
    ets:safe_fixtable( TableRef, true ),
    First = ets:first( TableRef ),
    try
        do_ets_each( TableRef, Fun, First )
    after
        ets:safe_fixtable( TableRef, false )
    end.

%% Recursive helper function for ets_each.
do_ets_each( _TableRef, _Fun, '$end_of_table' ) ->
    ok;    
do_ets_each( TableRef, Fun, Key ) ->
    Fun(ets:lookup( TableRef, Key )),
    do_ets_each( TableRef, Fun, ets:next( TableRef, Key ) ). 

%Setur game function
setUp()->
	compile:file(?GRAPHICS_SERVER),
	initial_servers(),
	ok.

% return node br name from : br@10.42.0.24 
get_node_name(NodeAtom)->
	list_to_atom(lists:nth(1, string:tokens(atom_to_list(NodeAtom), "@"))). %% Get Node Name As Atom

initial_servers() ->
	%% Compile all files in all nodes
	compile:file(gameManager),
	compile:file(mymath),
	FilesToCompile = [g, mines, unit, tank, resource, soldier, gameManager, base, harvester, mymath],
	Fun = fun(Node, Filename) -> rpc:call(Node, compile, file, [Filename]) end,
	[ Fun(Node, FileName) || Node <- nodes(), FileName <- FilesToCompile ],
	
	%% Set Lower Tick Time To Recover Faster On Node Down
	net_kernel:set_net_ticktime(8),
	%% Start gen_servers in all nodes
	Func = fun(Address) -> 
		Nodename = get_node_name(Address),
		rpc:call(Address, net_kernel, set_net_ticktime, [8]),
		rpc:call(Address, gameManager, start_server, [Nodename] ++ node_space(Nodename) ++ [?LIST_OF_NODES, ?WORLD_WIDTH, ?WORLD_HEIGHT]) 
	end,
	[ Func(X) || X <- nodes()],
	%% Set Monitor Nodes
	net_kernel:monitor_nodes(true),
	%% Start Graphics
	erlang:apply(?GRAPHICS_SERVER, start, []),	
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

start_game_to_nodes(NodesList) ->
	ets:delete_all_objects(?DBNAME),
	gameManager:createNewUnit(in_node(NodesList,[?BASE_OFFSET,?BASE_OFFSET]), [base, 1, [?BASE_OFFSET,?BASE_OFFSET]], 1),
 	gameManager:createNewUnit(in_node(NodesList,[?GLOBAL_X-1.3*?BASE_OFFSET,?GLOBAL_Y-1.3*?BASE_OFFSET]), [base, 2, [?GLOBAL_X-1.3*?BASE_OFFSET,?GLOBAL_Y-1.3*?BASE_OFFSET]], 1),
	Func=fun(X) ->  
		gameManager:createResources(X, 10),
		timer:sleep(55),
		gameManager:createResources(X, 10),
		timer:sleep(55),
		gameManager:createMines(X, 3),
		timer:sleep(55),
		gameManager:createMines(X, 3),
		timer:sleep(55)
	end,
	[ Func(Node) || Node<-[get_node_name(X) || X<-nodes()]],
	ok.

	
stop_game() ->
	gameManager:stop_server(tl),
	gameManager:stop_server(tr),
	gameManager:stop_server(bl),
	gameManager:stop_server(br),
	ok.


%% =================== Return Node Name Of [X,Y] ====================
in_node([],[X,Y]) -> 
	?DEBUG("IN NODE UNDEFINED : ~p~n", [[X,Y]]),
	undefined;
in_node([{NodeName, [XStart,XEnd],[YStart,YEnd]}|ListOfRanges], [X,Y]) ->
	if 
		(XStart =< X) and (XEnd > X) and (YStart =< Y) and (YEnd > Y) ->
			NodeName;
		true -> in_node(ListOfRanges, [X,Y])
	end.

%% When Node Is Down - Pinging Every 1 Seconds
ping_node(Node) ->
	receive
		pang -> ping_node(Node);
		pong -> ok
	after 1000 ->
		net_adm:ping(Node),
		ping_node(Node)
	end.

get_active_nodes_list(ListOfNodes)->
	lists:foldl(fun({Node, _, _}, Acc) -> Acc ++ [Node] end, [], ListOfNodes).

%Update node list in case thet nodeis down
change_nodes_list(Node, CurrentNodeList, down) ->
	TempNodeList = lists:keydelete(Node, 1, CurrentNodeList),
	case Node of
		tl -> 
			NewNodeList = lists:keystore(tr, 1, TempNodeList, {tr,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]});
		tr -> 
			NewNodeList = lists:keystore(tl, 1, TempNodeList, {tl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]});
		bl -> 
			NewNodeList = lists:keystore(br, 1, TempNodeList, {br,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]});
		br -> 
			NewNodeList = lists:keystore(bl, 1, TempNodeList, {bl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]})
	end,
	NewNodeList;

%update node list in case that node is up
change_nodes_list(Node, CurrentNodeList, up) ->
	case Node of
		tl -> 
			NewNodeList = lists:keystore(tr, 1, CurrentNodeList, {tr,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]}) ++
							 [{tl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]}];
		tr -> 
			NewNodeList = lists:keystore(tl, 1, CurrentNodeList, {tl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]}) ++
							[{tr,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]}]  ;
		bl -> 
			NewNodeList = lists:keystore(br, 1, CurrentNodeList, {br,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]}) ++
							[{bl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]}]  ;
		br -> 
			NewNodeList = lists:keystore(bl, 1, CurrentNodeList, {bl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]}) ++
							[{br,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]}]  
	end,
	NewNodeList.

%get node name that raplaces crashe node
replace_node(Atom) ->
	case Atom of
		tl -> tr;
		tr -> tl;
		bl -> br;
		br -> bl
	end.

%return node worl part of responsibility
node_space(Atom) ->
	case Atom of
		tl -> [?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE , ?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT];
		tr -> [?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH, ?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT];
		bl -> [?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE, ?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE];
		br -> [?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH, ?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]
	end.


updateNodes_delayed(NodeUp, UnitsToMove,NewListofNodes, Bases) ->
	receive
		_A -> ok
	after 1000 ->
		lists:foreach(fun({_Key,UnitRecord}) -> gameManager:moveUnitToNode(NodeUp, UnitRecord) end, UnitsToMove),
		%% Update Base PID On All Servers
		lists:foreach(fun({BasePID, Team}) -> gameManager:updateBase(NodeUp, BasePID, Team) end, Bases),
		%% Update List Of Nodes On All Relevant Servers
		lists:foreach(fun(ServerName) when ServerName=/=NodeUp ->
					  gameManager:updateNodes(ServerName, NewListofNodes);
				 (_Else) -> ok end, get_active_nodes_list(NewListofNodes))
	end.

  

