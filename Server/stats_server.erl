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

-define(STATS_SERVER, ?MODULE).
-define(REFRESH_TIME, 60*3).
-define(MAX_NO_UPDATE_TIME,5).
-define( LOG_DIR,"./logger/").
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
-record(event, {type, time, from, to, key, data}).

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
  gen_server:cast({global, ?STATS_SERVER}, stop).

export() -> 
  gen_server:cast({global, ?STATS_SERVER}, {export_db, isg_time:now_now()}).

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

  A = dets:open_file(?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}]),
  
  io:format("dets: ~p~n",[A]),
  {ok,DB} = A,
  dets:insert(DB, {0, { yalla_maccabi} }),

	%%PRO_DB = init_db(protocol),
    %%MANAGMENT_DB = init_db(management),
    %%Current_File_Version = get_file_version(),
    %%Node_ETS = get_nodes_id_from_file(Current_File_Version),
    

    Counters = #counters{numberOfManagementMsgSent = 0, numberOfManagementMsgReceived = 0, numberOfDataMsgSent = 0, numberOfDataMsgReceived = 0},
    %%print_nodes_list(Node_ETS),
    %%Self = self(),
    %%net_kernel:monitor_nodes(true),
   
    {ok, #state{counters = Counters, nodes_list = 0, db = DB}}.





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

%***************************************************************
%**********************   Data Server  Updated  ****************
%***************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a management message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{management_message, received_message}, [UTIME, Source, Destination, Type]}, State = #state{db = DB, counters = Counters}) ->
  dets:insert(DB, {UTIME, management_message, received_message,Source,Destination}),
  NumberOfManagementMsgReceived = Counters#counters.numberOfManagementMsgReceived,
  io:format("stats_server got report about: Incoming management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfManagementMsgReceived = NumberOfManagementMsgReceived + 1}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a management message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{management_message, sent_message}, [UTIME, Source, Destination, Type]}, State = #state{db = DB, counters = Counters}) ->
  dets:insert(DB, {UTIME, management_message, sent_message,Source,Destination}),
  NumberOfManagementMsgSent = Counters#counters.numberOfManagementMsgSent,
  io:format("stats_server got report about: Sent management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfManagementMsgSent = NumberOfManagementMsgSent + 1}}};

%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a data message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{data_message, received_message}, [UTIME, Source, Destination, Id]}, State = #state{db = DB, counters = Counters}) ->
  NumberOfDataMsgReceived = Counters#counters.numberOfDataMsgReceived,
  dets:insert(DB, {UTIME, data_message, received_message,Source,Destination, Id}),

	io:format("stats_server got report about: Incoming data msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfDataMsgReceived = NumberOfDataMsgReceived + 1}}};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a data message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({{data_message, sent_message}, [UTIME, Source, Destination ,Id]}, State = #state{db = DB, counters = Counters}) ->
	NumberOfDataMsgSent = Counters#counters.numberOfDataMsgSent,
  dets:insert(DB, {UTIME, data_message, sent_message,Source,Destination, Id}),

	io:format("stats_server got report about: Sent data msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
{noreply, State#state{counters = Counters#counters{numberOfDataMsgSent = NumberOfDataMsgSent + 1}}};



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
%%	----      Export database to file at end of the day  ----------
%%  ------------------------------------------------------------------
handle_cast({export_db, Last_Date}, State = #state{db = DB}) -> 
    io:format("stats_server got cast with export_db:~p~n", [Last_Date]),
  
    ETS = ets:new(temp, [ordered_set]),
    io:format("db:handle_cast:export_db: ETS is:~p~n", [ETS]),
    ETS = dets:to_ets(DB,ETS),
    io:format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),
    export_db(ETS, "tempurary_db_extraction.txt"),
    ets:delete(ETS),
  {noreply, State};



handle_cast(stop, State) -> 
    io:format("stats_server got stop Messages~n"),
    {stop, "Normal"};

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
handle_info(stop, State) ->
{stop, stop, State};

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
%%% DB's Daemon
%%%===================================================================


init_daemon(DB) -> 
  Now =isg_time:now_now(),
  loop(DB,Now).
  
loop(DB, Last_Date) ->
  receive
    stop -> io:format("db:deamon:loop:terminating deamon~n");
    now ->  gen_server:cast(DB, {export_db, Last_Date}), loop(DB, isg_time:now_now())
  after (1000*10) -> 
    Now =isg_time:now_now(),
    {{_Last_Year, _Last_Month, Last_Day}, _ } = calendar:gregorian_seconds_to_datetime(Last_Date),

    {{_Now_Year, _Now_Month, Now_Day}, _ } = calendar:gregorian_seconds_to_datetime(Now),
    if 
      Last_Day =:= Now_Day -> loop(DB, Now_Day);
      true -> io:format("~n~n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~n~n"), 
            gen_server:cast(DB, {export_db, Last_Date}), loop(DB, Now)
    end
  end.
    

%%%===================================================================
%%% Internal functions
%%%===================================================================





export_db(DB, File_Name) -> 
  
  A = file:open( ?LOG_DIR ++ File_Name, [append]),
  case A of
    {ok, FD} -> io:format("db:opened file~n"),copy_db_to_file2(DB, ets:first(DB), FD),
        %copy_db_to_file(DB, dets:first(DB), FD),
        file:close(FD),
        io:format("db: copied to file successfully.~n");
    {error,enoent} -> io:format("%%%%%%%******couldnt open file because of error ~p. file name is:~p.~n", [A, File_Name]);
    {error,badarg} -> file:open( ?LOG_DIR ++ File_Name, write), export_db(DB, File_Name);
    Else -> io:format("db:export_db: got Els:~p~n", [Else])
  end.
  
  
copy_db_to_file2(_DB, '$end_of_table', _FD) -> ok;
copy_db_to_file2( DB, Key, FD) ->
  
  [A] = ets:lookup(DB, Key),
  case A of
    {UTIME, management_message, received_message,Source,Destination, Type} ->
      STR = Destination ++ "Received Management Event Event At: " ++ isg_time:timeStamp_to_Time(UTIME) ++
            " From: " ++ Source ++ " Type: " ++ Type;
    {UTIME, management_message, sent_message,Source,Destination, Type} ->
      STR = Destination ++ "Received Management Event Event At: " ++ isg_time:timeStamp_to_Time(UTIME) ++
            " From: " ++ Source ++ " Type: " ++ Type;
    {UTIME, data_message, received_message,Source,Destination, Id} ->
      STR = "Event Key: " ++ integer_to_list(UTIME);
    {UTIME, data_message, sent_message,Source,Destination, Id} ->
      STR = "Event Key: " ++ integer_to_list(UTIME);
            
    Else -> io:format("db:copoy_db_to_file: ets:lookup got elst:~p~n", [Else]), UTIME = 0,  STR = ""
  end,
  
  LOG_STR =  integer_to_list(Key) ++ ")" ++ isg_time:timeStamp_to_Time(UTIME) ++"," ++ STR ++ "\n",
  _Ans = file:write(FD, LOG_STR),
  %try next log, if fails, try next one
  try
  copy_db_to_file2(DB, ets:next(DB, Key), FD)
  catch
  _:_ -> copy_db_to_file2(DB, ets:next(DB, ets:next(DB, Key) ), FD )
  end.

  

%%%===================================================================
%%% Nodes table ets functions
%%%===================================================================

