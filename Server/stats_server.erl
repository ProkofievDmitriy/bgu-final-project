%%%-------------------------------------------------------------------
%%% @author Deddy Zagury
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%%	This is the general statics server for the PLC+RF smart-meter project.
%%%
%%%	This server purpose is to collect and calculate statical data about the routing protocol used in any implentaion of PLC+RF smart meters.
%%%
%%%	The statical data that is gathered is:
%%%	
%%%		Number of data messages
%%%		Number of management message
%%%
%%% TODO:
%%%   calculate average send to receive time.
%%%   Log file
%%%   
%%% @end
%%% Created : 15. Jan 2017 1:40 AM
%%%-------------------------------------------------------------------
-module(stats_server).
-author("Deddy Zagury").

-behaviour(gen_server).

-define(ISG_SERVER, ?MODULE).
-define(REFRESH_TIME, 60*3).
-define(MAX_NO_UPDATE_TIME,5).
-define( LOG_DIR,"logger/").
-define( TEMP_DETS_FILE_DIR, ?LOG_DIR).
-define( TEMP_DETS_FILE, "temp_dets").
%% API
-export([start/0, stop/0]).



%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(counters, {numberOfManagementMsgSent, numberOfManagementMsgReceived, numberOfDataMsgSent, numberOfDataMsgReceived}).

-record(state, {counters, nodes_list, db ,file_version}).

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%  The initialization process includes:
%%      Creating DETS file for temporary storage of data.
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
    io:format("initiating server~n"),

    %%Messages_DB = init_db(management),
    %%Current_File_Version = get_file_version(),
    %%Node_DB = get_nodes_id_from_file(Current_File_Version),

  {_,DB} = dets:open_file(?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}]),

	%%PRO_DB = init_db(protocol),
    %%MANAGMENT_DB = init_db(management),
    %%Current_File_Version = get_file_version(),
    %%Node_ETS = get_nodes_id_from_file(Current_File_Version),
    

    Counters = #counters{numberOfManagementMsgSent = 0, numberOfManagementMsgReceived = 0, numberOfDataMsgSent = 0, numberOfDataMsgReceived = 0},
    %%print_nodes_list(Node_ETS),
    %%Self = self(),
    %%net_kernel:monitor_nodes(true),
   
    {ok, #state{counters = Counters, nodes_list = 0, db = DB}}.









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
%% Handling call messages
%%
%%		20.1.17 - no call messages support
%%
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
    io:format("stats_server:stopping~n"),
    {stop, normal, shutdown_ok, State};


handle_call(Req, From, State) ->
    io:format("stats_server handle_call: ~p from ~p~n", [Req,From]),
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
%**********************   Management Server  Updated  *******************************
%**************************************************************************************


%%--------------------------------------------------------------------
%%	----	node -> server cast management notifications	----------------------
%%  ------------------------------------------------------------------

%handle a call from node. node terminate it's run and let the server know about it
handle_cast({node_terminated, ID},  State) ->
	io:format("stats_server:handle_cast:node_terminated: ID is:~p~n", [ID]),
	%%update_node_status(ID, ?DOWN, State#state.nodes_list),
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


%***************************************************************
%**********************   Data Server  Updated  ****************
%***************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a data message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast( {{data_message, received_message}, [UTIME, Source, Destination]}, State = #state{db = DB, counters = Counters}) ->
  NumberOfDataMsgReceived = Counters#counters.numberOfDataMsgReceived,
  dets:insert(DB, {UTIME, "data_message", "received_message",Source,Destination}),

	io:format("stats_server got report about: Incoming msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfDataMsgReceived = NumberOfDataMsgReceived + 1}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a data message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{data_message, sent_message}, [UTIME, Source, Destination]}, State = #state{db = DB, counters = Counters}) ->
	NumberOfDataMsgSent = Counters#counters.numberOfDataMsgSent,
  dets:insert(DB, {UTIME, "data_message", "sent_message",Source,Destination}),

	io:format("stats_server got report about: Sent msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfDataMsgSent = NumberOfDataMsgSent + 1}}};

%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a management message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast( {{management_message, received_message}, [UTIME, Source, Destination]}, State = #state{db = DB, counters = Counters}) ->
  dets:insert(DB, {UTIME, "management_message", "received_message",Source,Destination}),
	NumberOfManagementMsgReceived = Counters#counters.numberOfManagementMsgReceived,
	io:format("stats_server got report about: Incoming management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfManagementMsgReceived = NumberOfManagementMsgReceived + 1}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a management message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{management_message, sent_message}, [UTIME, Source, Destination]}, State = #state{db = DB, counters = Counters}) ->
  dets:insert(DB, {UTIME, "management_message", "sent_message",Source,Destination}),
  NumberOfManagementMsgSent = Counters#counters.numberOfManagementMsgSent,
	io:format("stats_server got report about: Sent management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfManagementMsgSent = NumberOfManagementMsgSent + 1}}};




%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------
handle_cast({printStats}, State = #state{counters=Counters}) ->
	MessagesSent =  Counters#counters.numberOfManagementMsgSent +  Counters#counters.numberOfDataMsgSent,
	PercentManagement = Counters#counters.numberOfManagementMsgSent / MessagesSent,
	PercentLost = Counters#counters.numberOfDataMsgReceived / Counters#counters.numberOfDataMsgSent,

	io:format("printStats:~n"),
	io:format("MessagesSent: ~p~n",[MessagesSent]),
	io:format("PercentManagement: ~p~n",[PercentManagement]),
	io:format("PercentLost: ~p~n",[PercentLost]),

	io:format("numberOfDataMsgSent: ~p~n",[Counters#counters.numberOfDataMsgSent]),
	io:format("numberOfDataMsgReviced: ~p~n",[Counters#counters.numberOfDataMsgReceived]),
	io:format("numberOfManagementMsgSent: ~p~n",[Counters#counters.numberOfManagementMsgSent]),
	io:format("numberOfManagementMsgReviced: ~p~n~n",[Counters#counters.numberOfManagementMsgReceived]),

	{noreply,State};



%%  ------------------------------------------------------------------
%%	----   server -> server cast management notifications   ----------
%%  ------------------------------------------------------------------


handle_cast(Msg, State) -> 
    io:format("stats_server got cast with bad arg:~p~n", [Msg]),
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


handle_info(timer,State)->
  io:format("Timer event ~n"),
  erlang:send_after(?update_screen_time,self(),timer);


handle_info(Info, State) ->
  io:format("isg_server:handle_info: got somethjing:~p~n", [Info]),
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
terminate(_Reason, _State) ->
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






%%%===================================================================
%%% Internal functions
%%%===================================================================






%%%===================================================================
%%% Nodes table ets functions
%%%===================================================================

