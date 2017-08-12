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
-export([export/0, start/0, stop/0, stats_request/1]).



%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(counters, {numberOfRelayMsg, numberOfManagementMsgSent, numberOfManagementMsgReceived, numberOfDataMsgSent,
                    numberOfDataMsgReceived, data_msg_avg_time, data_msg_avg_relay_length}).

-record(state, {counters, nodes_list, db, dm_ets, file_version, time_base}).
%-record(event, {type, time, from, to, key, data}).

%%%===================================================================
%%% API
%%%===================================================================


stats_request(From) ->
  gen_server:cast({global, ?STATS_SERVER}, {states, From}).

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
  gen_server:cast({global, ?STATS_SERVER}, {export_db, stringTime(calendar:local_time())}).

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
  io:format("initiating stats server~n"),
  file:make_dir(?TEMP_DETS_FILE_DIR),
  A = dets:open_file(?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}, {type, duplicate_bag}]),
  DM_ets = ets:new(data_messages, [set]),
  io:format("dets: ~p~n",[A]),
  {ok,DB} = A,
  dets:insert(DB, {0, { yalla_maccabi} }),

  Counters = #counters{numberOfRelayMsg = 0,
                       numberOfManagementMsgSent = 0,
                       numberOfManagementMsgReceived = 0,
                       numberOfDataMsgSent = 0,
                       numberOfDataMsgReceived = 0,
                       data_msg_avg_time = 0},

  {ok, #state{counters = Counters, nodes_list = 0, db = DB, dm_ets = DM_ets, time_base = get_current_millis()}}.





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


handle_call({get_time_offset, NodeTime}, From, State) ->
    % Offset = NodeTime - State#state.time_base,
    Offset = NodeTime - get_current_millis(),
    io:format("stats_server handle_call: ~p , OFFSET = ~p for node ~p~n", [get_time_offset, Offset, From]),
    {reply, {ok, Offset}, State};

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

  handle_cast({states, From}, State = #state{counters=Counters}) ->
      {AvgTime,AvgLength} = average(State#state.dm_ets),
          io:format("Stats AvgTime: ~p~n",[AvgTime]),
          io:format("Stats AvgLength: ~p~n",[AvgLength]),
      io:format("COUNTERS :  ~p~n",[State#state.counters]),
      NewCounters = Counters#counters{data_msg_avg_time = AvgTime, data_msg_avg_relay_length = AvgLength},
      NewState = State#state{counters = NewCounters},
      From!{update_metrics, NewCounters},
      {noreply, NewState};

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
handle_cast({export_db, Time}, State = #state{db = DB}) ->
    io:format("stats_server got cast with export_db~n"),
    A = dets:close(DB),

    B = file:rename(?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db",?TEMP_DETS_FILE_DIR ++ "Last_Date"),
    io:format("RES: A ~p~nB: ~p~n",[A,B]),
    {ok,NewDB} = dets:open_file(?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ Time ++ ".db"}]),
  {noreply, State#state{db = NewDB}};

handle_cast(stop, State) ->
    io:format("stats_server got stop Messages~n"),
    {stop, normal,State};



  handle_cast({Type, Data}, State = #state{dm_ets = DM_ets, db = DB, counters = Counters}) ->
    dets:insert(DB, {Type , Data}),
    io:format("stats_server got Message ~p about ~p~n", [Type, Data]),
    case Type of
      relay ->
        update_dm_ets(DM_ets, relay, proplists:get_value(id, Data), proplists:get_value(utime, Data)),
        {noreply, State#state{counters = Counters#counters{numberOfRelayMsg = Counters#counters.numberOfRelayMsg + 1}}};

      {data_message, send_message} ->
        update_dm_ets(DM_ets, send_message, proplists:get_value(id, Data), proplists:get_value(utime, Data)),
        {noreply, State#state{counters = Counters#counters{numberOfDataMsgSent = Counters#counters.numberOfDataMsgSent + 1}}};

      {data_message, received_message} ->
        update_dm_ets(DM_ets, received_message, proplists:get_value(id, Data), proplists:get_value(utime, Data)),
        {noreply, State#state{counters = Counters#counters{numberOfDataMsgReceived = Counters#counters.numberOfDataMsgReceived + 1}}};

      {management_message, send_message} ->
        {noreply, State#state{counters = Counters#counters{numberOfManagementMsgSent = Counters#counters.numberOfManagementMsgSent + 1}}};

      {management_message, received_message} ->
        {noreply, State#state{counters = Counters#counters{numberOfManagementMsgReceived = Counters#counters.numberOfManagementMsgReceived + 1}}};
      Other ->
        io:format("stats_server got unresolved report ~p~n",[Other]),
        {noreply, State}
    end;



handle_cast(Msg, State) ->
    io:format("~n~n~nstats_server got cast with bad arg:~p~n~n~n", [Msg]),
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

handle_info(timer, State) ->
  erlang:send_after(1000,self(),timer),
  {noreply, State};

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


stringTime({{YY,MM,DD},{HH,MO,_}}) ->
  "DB" ++ integer_to_list(HH) ++ ": " ++ integer_to_list(MO) ++ "-" ++
  integer_to_list(DD) ++ "." ++ integer_to_list(MM) ++ "." ++ integer_to_list(YY).

export_db(DB, File_Name) ->
  dets:close(DB),


  dets:open_file(?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}]).


update_dm_ets(DM_ets, relay, Id, _UTIME) ->
  Curr = ets:lookup(DM_ets,Id),
  case Curr of
    [] ->
      ets:insert(DM_ets,{Id,{0, 0, 1}});
    [{Id,{SentTime, ReceivedTime, Relays}}] ->
      ets:insert(DM_ets,{Id,{SentTime, ReceivedTime, Relays + 1}});
    _ -> io:format(" CASE CLAUSE FAILED relay!!!!!!!!!!! Suka 2 AM !!!!!!~n"),
         ok
  end;

update_dm_ets(DM_ets, received_message, Id, UTIME) ->
  Curr = ets:lookup(DM_ets,Id),
  case Curr of
    [] ->
      ets:insert(DM_ets,{Id,{0, UTIME, 0}});
    [{Id,{SentTime,0, Relays}}] ->
      ets:insert(DM_ets,{Id,{SentTime, UTIME, Relays}});
    _ -> io:format(" CASE CLAUSE FAILED received_message!!!!!!!!!!! Suka 2 AM !!!!!!~n"),
         ok
  end;
update_dm_ets(DM_ets, send_message, Id, UTIME) ->
  Curr = ets:lookup(DM_ets,Id),
  case Curr of
    [] ->
      ets:insert(DM_ets,{Id,{UTIME,0 ,0}});
    [{Id,{0,ReceivedTime, Relays}}] ->
      ets:insert(DM_ets,{Id,{UTIME, ReceivedTime, Relays}});
    _ -> io:format(" CASE CLAUSE FAILED send_message!!!!!!!!!!! Suka 2 AM !!!!!!~n"),
         ok
  end.

%%%===================================================================
%%% Nodes table ets functions
%%%===================================================================

%{AvgTime,AvgLength} = average(State#state.dm_ets),
average(DB) ->
    average(DB, ets:first(DB),0.0,0,0.0,0).

  %%--------------------------------------------------------------------
  %% @private
  %% @doc
  %%  Calculate the average route length and the average data message
  %%  time to reach destination
  %% @spec average(DataBase, KeyToCalculate,CurrentTimeSum, CurrentTimeCount, CurrentLengthSum, CurrentLengthCount) ->
  %%                                   {AverageTime, AverageLength}
  %% @end
  %%--------------------------------------------------------------------
average(_, '$end_of_table',0.0,0, _,_) -> {0.0,0.0};
average(_, '$end_of_table',_,_, 0.0,0) -> {0.0,0.0};
average(_, '$end_of_table',SumTime,NumberTime, SumLength,NumberLength) -> {SumTime/NumberTime,SumLength/NumberLength};
average(DB, Key, SumTime, NumberTime, SumLength, NumberLength) ->
  [{Key,Data}] = ets:lookup(DB,Key),
  case Data of
    {StartTime,EndTime ,Relays} when StartTime > 0 andalso EndTime > 0 ->
    %  io:format("Line 414: ~p",[Data]),
      average(DB, ets:next(DB,Key), SumTime + (EndTime - StartTime), NumberTime+1, SumLength + Relays, NumberLength+1);
    _ ->
      average(DB, ets:next(DB,Key), SumTime, NumberTime, SumLength, NumberLength)
  end.


 get_current_millis() ->
     {Mega, Sec, Micro} = os:timestamp(),
     (Mega*1000000 + Sec)*1000 + round(Micro/1000).
