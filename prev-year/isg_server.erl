%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2015 1:40 AM
%%%-------------------------------------------------------------------
-module(isg_server).
-author("admin").

-behaviour(gen_server).

-define(ISG_SERVER, ?MODULE).
-define(REFRESH_TIME, 60*3).
-define(MAX_NO_UPDATE_TIME,5).
-define(UP, up).
-define(DOWN, down).

%% API
-export([start/0, stop/0, print_node_list/0, print_db/0, export_db/0,export_db_date/0, delete_db/0, delete_db/1, update_node_files/0, pause_node/1, resume_node/1, print_active/0, print_deactive/0]).

-export([get_nodes_id_from_file/1]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(state, {nodes_list, protocol_db, managment_db ,file_version, graphic}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).



start() ->
    start_link().
    
stop() -> 
		try_call({global,?ISG_SERVER}, stop).

print_node_list() ->
    try_cast({global,?ISG_SERVER}, print_node_list).

print_db()->
    try_cast({global,?ISG_SERVER}, {print_db,protocol}).

export_db_date() ->
      try_cast({global,?ISG_SERVER}, {export_db, protocol}).
export_db() ->
     try_cast({global,?ISG_SERVER}, {export_db, protocol, now}).
    
delete_db()-> delete_db(protocol).

delete_db(Type)->
     try_cast({global,?ISG_SERVER}, {delete_db, Type}). 

update_node_files() -> 
		try_call({global,?ISG_SERVER}, update_node_files).

print_active()-> try_cast({global,?ISG_SERVER}, db_print_activeate).
	
print_deactive()-> try_cast({global,?ISG_SERVER}, db_print_deactiveate).

pause_node(Node_ID) -> 
	try_cast({global,?ISG_SERVER}, {pause_node, Node_ID}). 	

resume_node(Node_ID) -> 
	try_cast({global,?ISG_SERVER}, {resume_node, Node_ID}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%%--------------------------------------------------------------------
%%	--------------			Wrrepers			----------------------
%%  ------------------------------------------------------------------
try_call(To, Msg) ->
	try gen_server:call(To, Msg) of
		Ans ->  Ans
	catch 
	_:_ -> io:format("isg_server:server_cast:error while trying to cast server ~p with MSG ~p . maybe it's closed~n",[To,Msg]), error
	end.	

try_cast(To, Msg) ->
	%io:format("try cast\t"),
	try gen_server:cast(To, Msg) of
		Ans ->  %io:format("Ans is:~p~n",[Ans]),
			Ans
	catch 
	_:_ -> io:format("isg_server:server_cast:error while trying to cast server ~p with MSG ~p . maybe it's closed~n",[To,Msg]), error
	end.	


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
     process_flag(trap_exit, true),
    io:format("initiating server~n"),
    Grapics_Pid = graphviz:start(13),
	%Grapics_Pid =2,
    PRO_DB = init_db(protocol),
    MANAGMENT_DB = init_db(managment),
    Current_File_Version = get_file_version(),
    Node_ETS = get_nodes_id_from_file(Current_File_Version),
    print_nodes_list(Node_ETS),
    Self = self(),
    start_node_deamon(Self),
    net_kernel:monitor_nodes(true),
   
    {ok, #state{ nodes_list = Node_ETS, protocol_db = PRO_DB, managment_db = MANAGMENT_DB, file_version = Current_File_Version, graphic = Grapics_Pid}}.







%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @endnode_termin
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).


handle_call(stop, _From, State) ->
    io:format("isg_server:stoooping~n"),
    {stop, normal, shutdown_ok, State};



%%--------------------------------------------------------------------
%%	----	server -> node call managemant notifications	----------------------
%%  ------------------------------------------------------------------
handle_call(update_node_files ,  _From, State) ->
	{File_List, Version} = get_file_list(),
	ETS = State#state.nodes_list,
	update_node_files2(ETS, ets:first(ETS),[], {File_List, Version}),
	{reply, ok, 
	#state{nodes_list = State#state.nodes_list, protocol_db = State#state.protocol_db,
	 				managment_db =	State#state.managment_db ,file_version = Version, graphic = State#state.graphic } };



%%--------------------------------------------------------------------
%%	----	node -> server call managemant notifications	----------------------
%%  ------------------------------------------------------------------
handle_call({register_node,  ID, Node_MAC_Addr, IP_ADDR, Node_Name}, _From, State)->
	io:format("isg_server: register_node:before Ans. ID is:~p~n ",[ID]),
	ETS = State#state.nodes_list,
    Ans = ets:lookup(ETS, ID),
    io:format("isg_server: register_node: Ans is:~p~n", [ Ans]),
    case Ans of
 	[ {ID, { _ ,_, _ , _, {last_seen, never}, _ }}] -> ANS2 = create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
	[ {ID, { _ ,_, _ , _, _, _ }}] -> ANS2 = update_node_status(ID, ?UP, ETS );
	[ {ID, { {mac_addr, not_reported} , _ ,_ , _, _ ,_  } } ] -> ANS2 =create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
	[ {ID, { _, {ip_addr, not_reported} , _ ,_ , _, _   } } ] -> ANS2 =create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
	[ {ID, { _, _, {name, not_reported} , _ ,_ , _	    } } ] -> ANS2 =create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
	[] -> io:format("isg_server: register_node: notfound~n"), ANS2 = create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
    	Else ->  io:format("isg_server: register_node: Ans is:Elose~n"), ANS2 = fuck
    end,
    io:format("isg_server: node ~p sent registeration request. ans is:~p~n", [Node_Name, ANS2]),
    {reply, Ans , State};


handle_call(Req, From, State) ->
    io:format("fuck me: ~p from ~p~n", [Req,From]),
    {reply, ignored, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).


%**************************************************************************************
%**********************   Managemant Server  Updated  *******************************
%**************************************************************************************


%%--------------------------------------------------------------------
%%	----	server -> node cast managemant notifications	----------------------
%%  ------------------------------------------------------------------
handle_cast({pause_node, Node_ID}, State) ->
		STR = "node_" ++ integer_to_list(Node_ID),
		Atom = list_to_atom(STR),
		try_cast({global,Atom}, pause_node),
   {noreply, State};


handle_cast({resume_node, Node_ID}, State) ->
		STR = "node_" ++ integer_to_list(Node_ID),
		Atom = list_to_atom(STR),
		try_cast({global,Atom}, resume_node),
   {noreply, State};



%%--------------------------------------------------------------------
%%	----	node -> server cast managemant notifications	----------------------
%%  ------------------------------------------------------------------




%handle_cast({register_node,  ID, Node_MAC_Addr, IP_ADDR, Node_Name}, State)->
 %   io:format("isg_server: register_node:before Ans~n "),
  %  ETS = State#state.nodes_list,
   % Ans = ets:lookup(ETS, ID),
%    io:format("isg_server: register_node: Ans is:~p~n", [ Ans]),
 %   case Ans of
%	[ {ID, { _ ,_, _ , _, {last_seen, never}, _ }}] -> ANS2 = create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
%	[ {ID, { _ ,_, _ , _, _, _ }}] -> ANS2 = update_node_status(ID, ?UP, ETS );
%	[] -> io:format("isg_server: register_node: notfound~n"), ANS2 = create_node_entry(ETS,ID, Node_MAC_Addr, IP_ADDR, Node_Name);
 %   	Else ->  io:format("isg_server: register_node: Ans is:Elose~n"), ANS2 = fuck
  %  end,
   % io:format("isg_server: node ~p sent registeration request. ans is:~p~n", [Node_Name, ANS2]),
    %{noreply , State};
 
%handle a call from node. node terminate it's run and let the server know about it
handle_cast({node_terminated, ID},  State) ->
	io:format("isg_server:handle_cast:node_terminated: ID is:~p~n", [ID]),
	update_node_status(ID, ?DOWN, State#state.nodes_list),
	%%graphviz:send_data([ID, none, none]),
	% A= ets:lookup(State#state.nodes_list, ID),
   	% [ {ID, { {mac_addr, MAC} ,{ip_addr, IP}, {name, NAME} , {active, _IS_ACTIVE}, {last_seen, _}, {file_version, _} }}] = A,
   	%timer:sleep(300),
    	%Ans = ets:delete(State#state.nodes_list, ID),
	%timer:sleep(300),
	%io:format("isg_server: call(node_terminated) : A is ~p.~n Ans is:~p.~n", [A, Ans]),
	%Now = isg_time:now_now(),
    	%ets:insert(State#state.nodes_list,{ID, { {mac_addr, MAC} ,{ip_addr, IP}, 
%    																					 {name, NAME} , {active, no}, {last_seen, Now} , {file_version, 1}
 %   																				  }}),
   {noreply, State}; 




handle_cast({new_version_accepted, ID, Version}, State) -> 
    io:format("isg_server: cast. new_version_accepted. ID ~p, ~n", [ID]),
    update_node_file_version(State#state.nodes_list, ID, Version),
    {noreply, State};




%**************************************************************************************
%**********************   Data Server  Updated  *******************************
%**************************************************************************************
%get sent massage tranfer notification. send it to main server
handle_cast({source_route_tabSize, Size} = Req, State) ->
  update_last_seen(State#state.nodes_list, 1),
  try_cast(State#state.protocol_db ,Req),
  CURL_STR = "curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'conn_nodes value=" ++ integer_to_list(Size) ++ "'",
  os:cmd(CURL_STR),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast( {newI,_Inew,Fsm_Name} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(Fsm_Name))),		%update last seen timestamp of current reporting node 
  try_cast(State#state.protocol_db ,Req),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast({inconsistency,Fsm_Name} = Req, State) ->	
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(Fsm_Name))),		%update last seen timestamp of current reporting node 
  try_cast(State#state.protocol_db ,Req),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast({pref_parent,Station_Name,Parent_Name,Transmission_medium} = Req , State) ->
 update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(Station_Name))),		%update last seen timestamp of current reporting node 
 try_cast(State#state.protocol_db ,Req),
 %update graphical server
 if Parent_Name =:= none -> Par_Name = none, Trans_Med = none;
 		    true -> Par_Name = list_to_integer(atom_to_list(Parent_Name)), Trans_Med = Transmission_medium 
 end,
 graphviz:send_data([list_to_integer(atom_to_list(Station_Name)),Par_Name , Trans_Med ]),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast({dio_out, StationNumber, _Index} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(StationNumber))),	%update last seen timestamp of current reporting node 
  try_cast(State#state.protocol_db ,Req),
  os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'mgmt_msg value=0'"),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast({dio_in, StationNumber, _Index} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(StationNumber))),	%update last seen timestamp of current reporting node 
  try_cast(State#state.protocol_db ,Req),
  os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'mgmt_msg value=0'"),
    {noreply, State};

    %get sent massage tranfer notification. send it to main server
handle_cast({dao_out, StationNumber, _Index} = Req, State) ->
 update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(StationNumber))),	%update last seen timestamp of current reporting node 
 try_cast(State#state.protocol_db ,Req),
 os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'mgmt_msg value=0'"),
    {noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({dao_in, StationNumber, _Index} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(StationNumber))),
  try_cast(State#state.protocol_db ,Req),
  os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'mgmt_msg value=0'"),
    {noreply, State};

%get data massage reception at station StationNumber
handle_cast( {new_Rank,StationNumber,Rank} = Req, State) ->
	 update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(StationNumber))),
	try_cast(State#state.protocol_db ,Req),
  	{noreply, State};

handle_cast(origin_up, State) ->
	os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'data_msgs value=0'"),
	{noreply, State};

handle_cast(receiver_up, State) ->
	os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'data_msgs value=1'"),
	{noreply, State};

handle_cast({data, ReceivingStationNumber, _SourceStation, _Power, _Temperature, _Humid} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(ReceivingStationNumber))),
  try_cast(State#state.protocol_db ,Req),
  {noreply, State};

%get received massage tranfer notification. send it to main server
handle_cast({rssi,Fsm_Name,_SenderNumber, _Index, _Transmission_medium,_Rssi} = Req, State) ->
  update_last_seen(State#state.nodes_list, list_to_integer(atom_to_list(Fsm_Name))),
   try_cast(State#state.protocol_db ,Req),
    {noreply, State};


%%--------------------------------------------------------------------
%%	----	server -> server cast managemant notifications	----------------------
%%  ------------------------------------------------------------------

handle_cast( check_nodes, State) ->
		Nodes = State#state.nodes_list,
		Now = isg_time:now_now(),
		{FL, _Curr_Version2} = get_file_list(),
		check_node_connection(Nodes, ets:first(Nodes), Now),
		%check_node_version(Nodes, ets:first(Nodes), State#state.file_version,FL),
   {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% node list functions - temp! %%
%%%%% Not for outside use %%%%%%%%
%%%%%%%% only for debug %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%call from user wants to see list of nodes known to server
handle_cast(node_list_print, State) ->
    print_nodes_list(State#state.nodes_list),
   {noreply, State};
%call from user wants to print server database
handle_cast(print_node_list, State) ->
    print_nodes_list(State#state.nodes_list),
     {noreply, State};


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% DB functions - temp! %%%%
%%%%%%%% Not for outside use %%%%%
%%%%%%%% only for debug %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%call from user wants to activate incoming messages printing
handle_cast(db_print_activeate,  State) ->
    try_cast(State#state.protocol_db, print_activeate),
    try_cast(State#state.managment_db, print_activeate),
    {noreply, State};

%call from user wants to activate incoming messages printing
handle_cast(db_print_deactiveate,  State) ->
	try_cast(State#state.protocol_db, print_deactiveate),
        try_cast(State#state.managment_db, print_deactiveate),
    {noreply, State};


%call from user wants to print server database
handle_cast({print_db, DB_TYPE},  State) ->
		case DB_TYPE of
			prtocol ->   DB = State#state.protocol_db;
			managment -> DB = State#state.managment_db;
			_Else -> DB = State#state.protocol_db
		end,
    try_cast(DB, print_db),
    {noreply, State};
   

%call from user wants to print server database
handle_cast({delete_db, DB_TYPE}, State) ->
    case DB_TYPE of
			protocol ->   DB = State#state.protocol_db;
			managment -> DB = State#state.managment_db;
			_Else -> DB = State#state.protocol_db
		end,
    try_cast(DB, delete_db),
    {noreply, State};


handle_cast({export_db, DB_TYPE, now}, State) ->
    io:format("isg_server: export data base now!!!!!~n"),
    case DB_TYPE of
      protocol ->   DB = State#state.protocol_db;
      managment -> DB = State#state.managment_db;
      _Else -> DB = State#state.protocol_db
    end,
  try_cast(DB,export_db),
  {noreply, State};

handle_cast({export_db, DB_TYPE}, State) -> 
    io:format("isg_server: export data base~n"),
    case DB_TYPE of
			protocol ->   DB = State#state.protocol_db;
			managment -> DB = State#state.managment_db;
			_Else -> DB = State#state.protocol_db
		end,
	%temp
    Now =isg_time:now_now(),
    {{_Last_Year, _Last_Month, Last_Day}, _ } = calendar:gregorian_seconds_to_datetime(Now),
	 io:format("ISG_SERVER:BEFORE TRY CAST OF db~n"),
    try_cast(DB,{export_db, Last_Day}),
   % try_cast(DB,export_db),
    {noreply, State};



handle_cast(Msg, State) -> 
    io:format("858585got cast with bad arg:~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).



handle_info({nodeup,Node_Name}, State) ->
  {noreply, State};

handle_info({nodedown,Node_Name_atom}, State) when is_atom(Node_Name_atom)->
  io:format("isg_server:handle_info: nodedown: Node_Name_atom is :~p~n", [Node_Name_atom]),
  Node_Name = atom_to_list(Node_Name_atom),
  io:format("isg_server:handle_info: nodedown: Node_Name_list is :~p~n", [Node_Name]),
  update_node_status(Node_Name, ?DOWN, State#state.nodes_list),
  io:format("before node_Int~n"),
  Node_Int = node_atom_to_int_id(Node_Name_atom),
  io:format("after node int~n"),
  %graphviz:send_data([Node_Int ,none , none ]),
  try graphviz:send_data([Node_Int ,none , none ]) of
	IDontFuckingCare -> io:format("success at try catch..... ~n"), sababa
  catch 
	A:B -> io:format("faild at try catch..... ~p\t~p~n", [A,B]), whyyyyyyyy
  end,
  io:format("after graphviz send data~n"),
  {noreply, State};


handle_info( {'EXIT',From,  Reason, X }, State)  ->
	io:format("~n~n~nisg_server:hhhhhhhhhhhhhhhhhhhhhhhandle_info:got Exit msg from: ~p reason:~p~n~n~n", [From, Reason]),
	{noreply, State};

handle_info( {'EXIT',From,  Reason }, State)  ->
	io:format("~n~n~nisg_server:handle_info:got Exit msg from: ~p reason:~p~n~n~n", [From, Reason]),
	{noreply, State};



handle_info(Info, State) ->
  io:format("isg_server:handle_info: got somethjing:~p~n!!!!!!!!@@@@@@@@@@@@@@@@@@@#################$$$$$$$$$$$$$$$$~n^^^^^^^^^^^^^&&&&&&&&&&&&&&&&&&&&&&&****************", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
	io:format("))))))))))))))))))))))))))))))))))))))))))~n"),
	%graphviz:stop(),
	try_call(State#state.managment_db, stop),
	try_call(State#state.protocol_db, stop),
	
	node_deamon!stop,
	io:format("isg server terminated!!!~n"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




%%--------------------------------------------------------------------
%%	----------------------		Deamon 			----------------------
%%  ------------------------------------------------------------------

start_node_deamon(Server) ->
	Pid = spawn(fun()-> node_deamon(Server,0) end),
	register(node_deamon, Pid).

node_deamon(Server,X) when X =:= ?REFRESH_TIME->
	io:format("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ demon woke up &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~n"),
	try_cast(Server, check_nodes), node_deamon(Server,0);
node_deamon(Server,X) ->
	receive
		stop -> bye
	after 1000 -> node_deamon(Server, X+1)
	end.





%%%===================================================================
%%% Internal functions
%%%===================================================================


check_node_version( _,'$end_of_table', _ ,_) -> done;
check_node_version(ETS, ID, Curr_Version,FL) -> 
	 A = ets:lookup(ETS, ID),
	 case A of
	 	[{ID, 
	 		{ {mac_addr, _MAC}, {ip_addr, _IP} , {name, Node_Name} , {active, yes}, {last_seen, _Then} ,{file_version,_Other_Version}    } 
	 	 } 
	 	] -> 	
	 			try_cast({global,list_to_atom(Node_Name)}, {file_update_waiting, {version, Curr_Version}, {file_list, FL} }),
	 			check_node_version(ETS, ets:next(ETS, ID), Curr_Version,FL);
	 	[{ID, 
	 		{ {mac_addr, _MAC}, {ip_addr, _IP} , {name, _Node_Name} , {active, no}, {last_seen, _Then} ,{file_version,_Version}    } 
	 	 } 
	 	] -> nevermined, check_node_version(ETS, ets:next(ETS, ID), Curr_Version,FL);
	 	[{ID, 
	 		{ {mac_addr, _MAC}, {ip_addr, _IP} , {name, _Node_Name} , {active, yes}, {last_seen, _Then} ,{file_version,Curr_Version}    } 
	 	 } 
	 	] -> up_to_date, check_node_version(ETS, ets:next(ETS, ID), Curr_Version,FL);

	 	Else -> io:format("isg:server:check_node_version:A is Else:~p~n", [Else])
	 end.



check_node_connection( _,'$end_of_table', _ ) -> done;
check_node_connection(ETS, ID, Now) -> 
	 A = ets:lookup(ETS, ID),
	 case A of
	 	[{ID, 
	 		{ {mac_addr, MAC}, {ip_addr, IP} , {name, Node_Name} , {active, yes}, {last_seen, Then} ,{file_version,Version}    } 
	 	 } 
	 	] -> Diff = Now - Then, 
	 		 if Diff > ?MAX_NO_UPDATE_TIME ->
	 		 	ets:insert(ETS, {ID, {{mac_addr, MAC}, {ip_addr, IP} , {name, Node_Name} , {active, no}, {last_seen, Then} ,{file_version,Version} } }),
	 		 	check_node_connection(ETS, ets:next(ETS, ID), Now);
	 		 true -> ok, check_node_connection(ETS, ets:next(ETS, ID), Now) end;
	 	[{ID, 
	 		{ {mac_addr, _MAC}, {ip_addr, _IP} , {name, _Node_Name} , {active, no}, {last_seen, _Then} ,{file_version,_Version}    } 
	 	 } 
	 	] -> nevermined, check_node_connection(ETS, ets:next(ETS, ID), Now);
	 	Else -> io:format("isg:server:check_node_connection:A is Else:~p~n", [Else])
	 end.



get_file_version() -> todo, 1.

get_file_list() -> todo, {file_list, version}.


send_update_alert_to_nodes([],_,_) -> done; 
send_update_alert_to_nodes([{ID,Name} | Rest] , {FL, Version} = Files, ETS) -> 
	A = try_cast({global, Name}, {file_update_waiting, {version, Version}, {file_list, FL} } ),
	if A =:= error -> node_not_available(ETS, ID);
					  true ->  good 
	end, send_update_alert_to_nodes(Rest, Files, ETS).

update_node_files2(ETS, '$end_of_table', L, Files) -> send_update_alert_to_nodes(L, Files, ETS);

update_node_files2(Nodes_ETS, ID, L, {New_Version, _Files_List } = Files) -> 
	A = ets:lookup(Nodes_ETS, ID),
	case A of
        [{ID, { _ ,  _ , _ , _, _, {file_version, New_Version} } } ] -> version_already_updated, 
        																update_node_files2(Nodes_ETS, ets:next(Nodes_ETS,ID), L, Files); 
        [{ID, { _ , _ ,{name, Name} , {active, yes}, _ , _ }}]  ->
        		Name = {global, list_to_atom(Name) },  
        		L1 = L ++ [{ID,Name}], update_node_files2(Nodes_ETS, ets:next(Nodes_ETS,ID), L1, Files);
        not_found -> node_not_at_list, update_node_files2(Nodes_ETS, ets:next(Nodes_ETS,ID), L, Files);
        Else -> io:format("wtfff???Else is:~p~n", [Else]), update_node_files2(Nodes_ETS, ets:next(Nodes_ETS,ID), L, Files)
    end.





%%%===================================================================
%%% Nodes table ets functions
%%%===================================================================


%try to register node at server's list. return one of 3: node_registered, node_already_registered, node_not_at_list
create_node_entry(Nodes_ETS, ID, Node_MAC_Addr, IP_ADDR, Node_Name) ->
	io:format("isg_server:create_node_entry: inside~n"),
	A = ets:lookup(Nodes_ETS, ID),
	case A of
        %first initiation of node
        [{ID, { {mac_addr, _MAC} , {ip_addr, _IP_ADDR} ,{name, _NAME} , {active, no}, {last_seen, _} ,{file_version,_} }}]  -> 
        		Now = isg_time:now_now(),  
        		ets:insert(Nodes_ETS, {ID, { {mac_addr, Node_MAC_Addr}, {ip_addr, IP_ADDR} , 
        																 {name, Node_Name} , {active, yes}, {last_seen, Now} ,{file_version,1}
        															 } }), 
        		node_registered;
        %already active. node probebly rebooted
        [{ID, { {mac_addr, MAC}, { ip_addr, IP }, {name, _NAME} , {active, yes}, {last_seen, _} ,{file_version,Version} }}] ->
         		Now = isg_time:now_now(),
          	ets:insert(Nodes_ETS, {ID, { {mac_addr, MAC}, {ip_addr, IP} , {name, Node_Name} , {active, yes}, {last_seen, Now} ,{file_version,Version} } }), 
         		node_already_registered;
        not_found -> node_not_at_list;
        Else -> io:format("wtfff???Else is:~p~n", [Else]), else
    end.


node_atom_to_int_id(Node_Atom) when is_atom(Node_Atom)-> node_name_to_int_id(atom_to_list(Node_Atom));
node_atom_to_int_id(Node_Atom) -> not_atom.
node_name_to_int_id(Node_name) when is_list(Node_name)->
	Node = string:sub_word(Node_name, 1, $@),
	io:format("node is:~p~n", [Node]),
	Key = string:substr(Node, 6), 
	io:format("Key is:~p~n", [Key]),
	Key_int = list_to_integer(Key),
	io:format("Key_int is:~p~n", [Key_int]), Key_int;
node_name_to_int_id(Node_name) -> not_string.

update_node_status(Node_name, Status, ETS) when is_atom(Node_name)-> update_node_status(list_to_integer(atom_to_list(Node_name)), Status, ETS);
update_node_status(Node_name, Status, ETS) when is_integer(Node_name)-> Name = "node_"++ integer_to_list(Node_name) ++ "@blabla", io:format("ETS is:~p~n", [ETS]), update_node_status(Name, Status, ETS);
update_node_status(Node_name, Status, ETS)->
	io:format("isg_server:update_node_status: got Node_name:~p, Status:~p, ETS:~p~n", [Node_name,Status,ETS]),
	Node = string:sub_word(Node_name, 1, $@),
	Key_int = node_name_to_int_id(Node_name),
	io:format("ETS is:~p~n", [ETS]),
	Ans = ets:lookup(ETS, Key_int),
	io:format("Ans is:~p~n", [Ans]),
	io:format("after ans~n"),
	LS = isg_time:now_now(),
	io:format("after now~n"),
	case Ans of 
		 [{Key2, { {mac_addr, MAC}, { ip_addr, IP }, {name, NAME} , {active, yes}, {last_seen, _} ,{file_version,Version} }}] -> if Status == ?UP -> ACTIVE = yes;  true -> ACTIVE = no end,
			ets:insert(ETS, {Key_int, { {mac_addr, MAC}, { ip_addr, IP }, {name, NAME} , {active, ACTIVE}, {last_seen, LS} ,{file_version,Version} }} ), connected;
		[{Key2, { {mac_addr, MAC}, { ip_addr, IP }, {name, NAME} , {active, no}, {last_seen, _} ,{file_version,Version} }}] -> if Status == ?UP -> ACTIVE = yes;  true -> ACTIVE = no end,
			ets:insert(ETS, {Key_int, { {mac_addr, MAC}, { ip_addr, IP }, {name, NAME} , {active, ACTIVE}, {last_seen, LS} ,{file_version,Version} }} ),
			io:format("isg_server:update_node_status: got disconnection alert from node ~p, but it was already inactive at table!~n", [Node]), connected;
		not_found -> io:format("isg_server:update_node_status: error!!!Ans is:~p. coldent find node ~p~n", [Ans,Node]), not_connected;
		Else -> io:format("isg_server:update_node_status: got Else: ~p~n", [Else]), not_connected
	end, io:format("after case~n").
    
    

print_nodes_list(Node_ETS) ->  
    io:format("List Of Nodes at system:~nindex\tMAC Address\t\tIP ADDRESS\t\tName\t\tis active?\tLast Seen\t\tFile_Version~n"),
    print_node(Node_ETS, ets:first(Node_ETS)).

print_node(_, '$end_of_table') -> io:format("done!~n");
print_node(Node_ETS, Key) -> [{Key, { {mac_addr, MAC} ,{ip_addr, IP}, {name, NAME} , {active, IS_ACTIVE}, {last_seen, LS}, {file_version, Version}  }}] = ets:lookup(Node_ETS, Key), 
		LS2 = isg_time:timeStamp_to_Time(LS),
    io:format("~p)\t~p\t~p\t~p\t~p\t\t~p\t\t~p~n", [Key, MAC, IP,  NAME, IS_ACTIVE, LS2, Version]),
    print_node(Node_ETS, ets:next(Node_ETS, Key)).
    


get_nodes_id_from_file(Version) ->
		Nodes_File = "nodes_id.txt",
    io:format("getting nodes from file (~p)~n", [Nodes_File]),
    ETS = ets:new(name, [ordered_set]),
    ANS = file:open(Nodes_File, read),
    try {ok, _FD1} = ANS of
        {ok, FD} -> get_Nodes(ETS, FD, file:read_line(FD), Version),  %if successful - return ets
                    ETS        
    catch
        {error,enoent} ->     %if failed - delete ets table and return not_initiated atom instead of ETS pointer
            io:format("couldnt open file~n"), ets:delete(ETS), not_initiated
    end.
    

init_db(Name) -> io:format("init DB~n"),
    {ok, P} = db:start(Name), P.
    
get_Nodes(_ETS, _FD, eof, _ ) -> done;
get_Nodes(_,_, {error, _Reason} ,_) -> io:format("wtf???");
get_Nodes(ETS, FD, {ok, Line}, Version ) -> 
    STR_ID = string:sub_word(Line, 1, $,),
    STR_Mac = string:sub_word(string:sub_word(Line, 2, $,), 1, $\n),
    
    ets:insert(ETS, { list_to_integer(STR_ID), { {mac_addr, STR_Mac} ,{ip_addr, not_reported}, {name, not_reported},  {active, no}, {last_seen, never} , {file_version, Version} }  } ),
    get_Nodes(ETS, FD, file:read_line(FD), Version ).


node_not_available(ETS, ID) ->
    [{ID, { {mac_addr, MAC} , { ip_addr, IP},{name, NAME} , 
    				{active, _IS_ACTIVE}, {last_seen, LS} , {file_version, Version} 
    			}}]				 = ets:lookup(ETS, ID),
   ets:insert(ETS, {ID, { {mac_addr, MAC} , {ip_addr, IP}, {name, NAME} , {active, no}, 
   												{last_seen, LS }, {file_version, Version}  
   											}  } ). 	
		

update_node_file_version(ETS, ID, Version) -> 
    A = ets:lookup(ETS, ID),
    case A of
    	[{ID, { {mac_addr, MAC} , { ip_addr, IP},{name, NAME} , 
    				{active, _IS_ACTIVE}, {last_seen, _}, {file_version, _}
    	}}]	-> 	Now = isg_time:now_now(),
    			ets:insert(ETS, {ID, { {mac_addr, MAC} , {ip_addr, IP}, {name, NAME} , {active, yes}, {last_seen, Now }, {file_version, Version} 
    			}  } );
    	Else-> io:format("isg_Server:update_node_file_version:ets:lookup got Else:~p~n", [Else])
    end.
	
update_last_seen(ETS, ID) -> 
    [{ID, { {mac_addr, MAC} , { ip_addr, IP},{name, NAME} , 
    				{active, _IS_ACTIVE}, {last_seen, _}, {file_version, Version}  
    			}}]				 = ets:lookup(ETS, ID),
    			
    Now = isg_time:now_now(),
    
    ets:insert(ETS, {ID, { {mac_addr, MAC} , {ip_addr, IP}, {name, NAME} , 
    											 {active, yes}, {last_seen, Now }, {file_version, Version}  
    										 }  } ).
	
