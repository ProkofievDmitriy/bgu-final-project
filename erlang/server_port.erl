%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2015 1:40 AM
%%%-------------------------------------------------------------------
-module(server_port).
-author("admin").

-behaviour(gen_server).

-define(ISG_SERVER_NAME, isg_server).
-define(ISG_SERVER_ADDR, "132.72.110.133").
-define(SERVER_PORT, server_port).
-define(TIME_CHECK_SERVER_CONNECTION_SECS, 60).

%% API
-export([start_link/4, send/3, send/1, register_at_isg_server/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(state, {node_name, node_mac_addr, node_id}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% sending functions
%% @end
%%--------------------------------------------------------------------


start_link(ID, MAC, IP,  Node_Name) ->
	ATOM_Name = list_to_atom( Node_Name),
	io:format("atom name of server port is: \t\t~p~n", [ATOM_Name]),
	%io:format("server_port:start_link:. globally registerged ar: ~p.~n", [global:registered_names()]),
  	gen_server:start_link({global, ATOM_Name}, ?MODULE, [ID, MAC,IP, Node_Name], []).

send(received_packet,Index, Packet) -> 
	?SERVER_PORT ! {cast , {received_packet_updae,Index, Packet}};
	


send(sent_packet,Index, Packet) ->
	?SERVER_PORT ! {cast ,{sent_packet_updae, Index, Packet}}.
	
send(Update) ->
	%io:format("server_port:send(Update) Update is:~p~n", [Update]),
	?SERVER_PORT ! {cast,Update}.


register_at_isg_server()-> 
	Pid = self(),
	?SERVER_PORT ! {call,Pid, register_node},
	receive
	Ans -> Ans
	end.
	%gen_server:call({global,?SERVER_PORT}, register_node).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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

init([ID, MAC,IP, Node_Name]) ->
	Server_Name = list_to_atom(atom_to_list(?ISG_SERVER_NAME) ++ "@" ++ ?ISG_SERVER_ADDR),
	Ans = net_kernel:connect_node(Server_Name),
	timer:sleep(1000),
	case Ans of 
		true -> io:format("server_port:init:connected to server erlang-wise (Ans is true)~n"),
						%Ans2 = try_server_call({global, isg_server}, {register_node, ID, MAC,IP, Node_Name});
						Ans2 = try_server_call({global, isg_server}, {register_node, ID, MAC,IP, Node_Name});
		false -> Ans2 = no_attempt,
						io:format("server_port:init:coulden't connect to server erlang-wise (Ans is flase!!!!!!!!!!)~n");
		_Else -> Ans2 = zoobi
	end,
	io:format("registered node at isg_server, Ans is ~p, Ans2 is:~p....!!!~n", [Ans, Ans2]),
	%TODO - add case for Ans2
	start_deamon(),
	
 	{ok, #state{node_name = Node_Name, node_mac_addr = MAC, node_id = ID}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%%	--------------			Wrrepers			----------------------
%%  ------------------------------------------------------------------
try_server_call(To, Msg) ->
	try gen_server:call(To, Msg) of
		Ans ->  Ans
	catch 
	_:_ -> io:format("server_port:server_call:error while trying to call server . maybe it's closed~n")
	end.
	
try_server_cast(To, Msg) ->
	try gen_server:cast(To, Msg) of
		Ans ->  Ans
	catch 
	_:_ -> io:format("server_port:server_cast:error while trying to cast server . maybe it's closed~n")
	end.	







-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).




%call for termination of server port gen server
handle_call(stop, _From, State) ->
	{stop, normal, shutdown_ok, State};


%%--------------------------------------------------------------------
%%	----	node -> server call managemant notifications	----------------------
%%  ------------------------------------------------------------------


handle_call( register_node, _From, State) ->
	Server_Name = list_to_atom(atom_to_list(?ISG_SERVER_NAME) ++ "@" ++ ?ISG_SERVER_ADDR),
	Ans = net_kernel:connect_node(Server_Name),
	ID = State#state.node_id,
	io:format("~n~nserver_port:handle_call:register_node: try to register at server. my ID is:~p~n~n", [ID]),
	case Ans of 
		true -> X = try_server_call({global,?ISG_SERVER_NAME}, {register_node,ID ,  State#state.node_mac_addr, State#state.node_name});
		false -> X = server_not_responding;
		_Else -> X = zoobi2
	end, 
	case X of
		server_not_responding -> {reply, failed, State};
		node_registered -> {reply, success, State};
		node_already_registered -> {reply, success, State};
		node_not_at_list -> {reply, failed, State};
		Else -> io:format("server_port:handle_call:register_nade: got Else:~p~n", [Else])
	end;


handle_call(resume_notification, _From, State) ->
	try_server_call({global,?ISG_SERVER_NAME}, {register_node, State#state.node_id,  State#state.node_mac_addr, State#state.node_name}),
	{reply, ok, State};


%termination announcment from supervisor only (TODO!). send syncronious msg to server
handle_call(termination_notification, _From, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, {node_terminated, State#state.node_id}),
	{reply, ok, State}.


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



%%--------------------------------------------------------------------
%%	----	Server -> node  cast managemant notifications	----------------------
%%  ------------------------------------------------------------------
handle_cast(pause_node, State) ->
		io:format("server_port:handle_cast: pause node~n"),
		gen_server:cast(isg_node, pause),
  	{noreply, State};

handle_cast(resume_node, State) ->
		io:format("server_port:handle_cast: resume node~n"),
		gen_server:cast(isg_node, resume),
  	{noreply, State};


handle_cast({file_update_waiting, {version, _Version}, {file_list, _FL} } = Req, State) ->
		gen_server:cast(isg_node, Req),
  	{noreply, State};


%%--------------------------------------------------------------------
%%	----	node -> server cast managemant notifications	----------------------
%%  ------------------------------------------------------------------
handle_cast({new_version_accepted, _ID, _Version} = Req, State) ->
		try_server_cast({global, ?ISG_SERVER_NAME}, Req),
  	{noreply, State};



%**************************************************************************************
%**********************		Data Server  Updated	*******************************
%**************************************************************************************

%get sent massage tranfer notification. send it to main server
handle_cast({source_route_tabSize, _Size} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast( {newI,_Inew,_Fsm_Name} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({inconsistency,_Fsm_Name} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({pref_parent,_Station_Name,_Parent_Name,_Transmission_medium}  = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({dio_out, _StationNumber, _Index} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({dio_in, _StationNumber, _Index} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get sent massage tranfer notification. send it to main server
handle_cast({dao_out, _StationNumber, _Index} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};


%get sent massage tranfer notification. send it to main server
handle_cast({dao_in, _StationNumber, _Index} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get data massage reception at station StationNumber
handle_cast({new_Rank,StationNumber,_Rank} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req), 
  	{noreply, State};

%get data massage reception at station StationNumber
handle_cast({data, StationNumber, Payload} = Req, State) ->
	[Power, Temperature, Humid, ID] = Payload,
	New_Req = {data, StationNumber, ID, Power, Temperature, Humid },
	try_server_cast({global,?ISG_SERVER_NAME}, New_Req), 
  	{noreply, State};

handle_cast(counter_origen_up, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, origin_up),
	{noreply, State};

handle_cast(counter_receiver_up, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, receiver_up),
	{noreply, State};

%get received massage tranfer notification. send it to main server
handle_cast({rssi,_Fsm_Name,_SenderNumber,_Index,_Transmission_medium,_Rssi} = Req, State) ->
	try_server_cast({global,?ISG_SERVER_NAME}, Req),
  	{noreply, State};

handle_cast( Req, State) ->
	io:format("wtffffffff???????? cast doesnt matching anything. req is:~p~n", [Req]),
	
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
handle_info(_Info, State) ->
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
		%notify isg_server for node termination
	try_server_cast({global,?ISG_SERVER_NAME}, {node_terminated, State#state.node_id}),
	stop_deamon(),
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


%%%==================================================================
%%%	deamon
%%%==================================================================
start_deamon() -> 
	Pid = self(),
	Deamon_Pid = spawn( fun()-> local_to_global_server_deamon(Pid,0) end),
	register(?SERVER_PORT, Deamon_Pid).

local_to_global_server_deamon(Pid, I) when I == ?TIME_CHECK_SERVER_CONNECTION_SECS -> 
	io:format("~n~n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~nlocal_to_global_server_deamon::: ensure connections to server~n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^~n~n"),
	Server_Name = list_to_atom(atom_to_list(?ISG_SERVER_NAME) ++ "@" ++ ?ISG_SERVER_ADDR),
	try net_kernel:connect_node(Server_Name) of
		Ans2 when is_atom(Ans2) -> io:format("ans for deamon's reconnect request from server is:~p~n",[Ans2]);
		Ans ->good
	
	catch
	A:B ->  io:format("error at net_kernel:connect node:~p,~p~n",[A,B])
	
	end,
	local_to_global_server_deamon(Pid, 0);
local_to_global_server_deamon(Pid, I) ->
	receive
		stop -> deamon_terminated;
		{call,From, Msg} ->	 Ans = gen_server:call(Pid, Msg),  
					From ! Ans,					
					local_to_global_server_deamon(Pid,I);
		{cast, Msg} ->  gen_server:cast(Pid, Msg), local_to_global_server_deamon(Pid,I)
	after 1000 -> local_to_global_server_deamon(Pid,I+1)
	end.
stop_deamon() ->
	?SERVER_PORT!stop.

%%%===================================================================
%%% Internal functions
%%%===================================================================




%send a request to supervisor, to clarify if node work is normal. return ok if its normal, error otherwise
check_intactness() -> ok.
