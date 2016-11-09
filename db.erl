-module(db).

%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2015 1:40 AM
%%%-------------------------------------------------------------------
-behaviour(gen_server).

%% API
-export([start/1, insert/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define( LOG_DIR,"logger/").
-define( TEMP_DETS_FILE_DIR, ?LOG_DIR).
-define( TEMP_DETS_FILE, "temp_dets").

-record(state, {db, i,  deamon, print}).

%%%===================================================================
%%% API
%%%===================================================================



insert(PID, {packet,Received_or_Sent,Index, Node_ID, Packet})->
	%io:format("db module:insert. db ID is:~p~n", [PID]),
	gen_server:cast(PID, {packet, Received_or_Sent,Index, Node_ID, Packet}).
start(Name) -> start_link(Name).



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(Name) ->
  gen_server:start_link({local, Name}, ?MODULE, [], []).

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


init([]) ->
	{_,DETS} = dets:open_file(?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}]),
	dets:insert(DETS, {0, { yalla_maccabi} }),
	%ETS = ets:new(db, [ordered_set]),
	%ets:insert(ETS, {0, {hooopa}}),
	P1 = self(),
	P = spawn(fun() -> init_deamon(P1) end),
	io:format("opened dets. db initiated~n"),
  	{ok, #state{db = DETS , i = 1, deamon = P, print = no} }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};


handle_call(_, _From, State) ->
	io:format("wwwwwwwwtf????~n"),
	{reply, ok, State}.






%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @enddio_out message, StationNumber '2' Index 85

%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% DB General Functions %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_cast(delete_db, State) ->
	io:format("db:deleting all data~n"),
	dets:delete_all_objects(State#state.db),
	dets:close(State#state.db),
	{_,DETS} = dets:open_file(?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"}]),
	dets:insert(DETS, {0,{new_start}}),
	{noreply, #state{db = DETS, i=1, deamon=State#state.deamon, print=State#state.print }};


handle_cast(print_db, State) ->
	DB = State#state.db,
	ETS = ets:new(temp, [ordered_set]),
	ETS = dets:to_ets(DB,ETS),
	io:format("Time\t\tMsg Type\tRec/Sent\t\tIndex\t\tSrc\t\tDest\t\tChannel\t\tRSSI~n"),
	print_db(DB, ets:first(ETS)),
	ets:delete(ETS),
	{noreply, State};


handle_cast(print_deactiveate, State) ->
	{noreply, #state{db = State#state.db, i=State#state.i, deamon=State#state.deamon, print=no}};

handle_cast(print_activeate, State) ->
	{noreply, #state{db = State#state.db, i=State#state.i, deamon=State#state.deamon, print=yes}};


handle_cast(export_db, State) -> io:format("export db without now~n"),
	DB = State#state.db,
	io:format("db:handle_cast:export_db: DB is:~p~n", [DB]),
	ETS = ets:new(temp, [ordered_set]),
	io:format("db:handle_cast:export_db: ETS is:~p~n", [ETS]),
	ETS = dets:to_ets(DB,ETS),
	io:format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n"),
	export_db(ETS, "tempurary_db_extraction.txt"),
	ets:delete(ETS),
	{noreply, State};


%%export DB at a new day. save it to file and reset db with first entry {0, {}}
handle_cast({export_db, Then}, State) ->
	io:format("db:handle_cast:{export_db, Then}~n"),
	{{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Then),
	File_Name = "db_protocol_data_" ++ integer_to_list(Day) ++ "_" ++ integer_to_list(Month) ++ "_" ++ integer_to_list(Year) ++ ".txt",
	DB = State#state.db,
	ETS = ets:new(temp, [ordered_set]),
	ETS = dets:to_ets(DB,ETS),
	export_db(ETS, File_Name),
	ets:delete(ETS),
	dets:delete_all_objects(DB),
	dets:insert(DB, {0, {}}),
	
	io:format("db:cast(export_db). File name is: ~p~n", [File_Name]),
	{noreply, #state{db = State#state.db, i=1, deamon=State#state.deamon, print=State#state.print}};



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Protocol Msgs Functions %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_cast( Req, State) when is_tuple(Req) ->
    DB = State#state.db,
    %Last = ets:last(DB) + 1,
    Last = State#state.i,
    Time = isg_time:now_now(),
    New_Req = erlang:insert_element(tuple_size(Req) +1, Req,Time),
    if State#state.print =:= yes -> io:format("db:handle_cast:Req: inserting Req:~p to db~n", [New_Req]);
	true -> cont end,
    dets:insert(DB, {Last, New_Req}),
    {noreply, #state{db = State#state.db, i = State#state.i + 1, deamon = State#state.deamon, print=State#state.print } };





handle_cast(_Request, State) ->
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

  
terminate(_Reason, State) ->
	State#state.deamon ! stop,
	Now = isg_time:now_now(),
	{{Year, Month, Day}, _} = calendar:gregorian_seconds_to_datetime(Now),
	File_Name = "db_protocol_data_" ++ integer_to_list(Day) ++ "_" ++ integer_to_list(Month) ++ "_" ++ integer_to_list(Year) ++ ".txt",
	DB = State#state.db,
	ETS = ets:new(temp, [ordered_set]),
	ETS = dets:to_ets(DB,ETS),
	export_db(ETS, File_Name),
	ets:delete(ETS),
	io:format("BD terminated!~n"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.




%%%===================================================================
%%% DB's Deamon
%%%===================================================================


init_deamon(DB) -> 
	Now =isg_time:now_now(),
	loop(DB,Now).
	
loop(DB, Last_Date) ->
	receive
		stop -> io:format("db:deamon:loop:terminating deamon~n")
	after (1000*10) -> 
		Now =isg_time:now_now(),
		{{_Last_Year, _Last_Month, Last_Day}, _ } = calendar:gregorian_seconds_to_datetime(Last_Date),
		{{_Now_Year, _Now_Month, Now_Day}, _ } = calendar:gregorian_seconds_to_datetime(Now),
		if Last_Day =:= Now_Day -> loop(DB, Now);
				    true -> io:format("&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~n~n~n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&~n~n"), 
						gen_server:cast(DB, {export_db, Last_Date}), gen_server:call(DB, delete_db), loop(DB, Now)
		end
	end.
		
		



%%%===================================================================
%%% Internal functions
%%%===================================================================

       

copy_db_to_file2(_DB, '$end_of_table', _FD) -> done;
copy_db_to_file2( DB, Key, FD) ->
	
	[A] = ets:lookup(DB, Key),
	case A of
		{Key, { source_route_tabSize, Size, Time } }->  STR = atom_to_list(source_route_tabSize) ++ "," ++ integer_to_list(Size);
		{Key, { newI,Inew,Fsm_Name, Time } }->  STR = atom_to_list(newI) ++ "," ++ integer_to_list(Inew) ++ "," ++ atom_to_list(Fsm_Name);
		{Key, { inconsistency,Fsm_Name, Time }}->  STR = STR = atom_to_list(inconsistency) ++ "," ++ atom_to_list(Fsm_Name);
		{Key, { pref_parent,Station_Name,none,Transmission_medium, Time }} ->  STR = STR = atom_to_list(pref_parent) ++ "," ++ atom_to_list(Station_Name) ++ ",none,none"; 
		{Key, { pref_parent,Station_Name,Parent_Name,Transmission_medium, Time }} ->  STR = STR = atom_to_list(pref_parent) ++ "," ++ atom_to_list(Station_Name) ++ "," ++ atom_to_list(Parent_Name) ++ "," ++ integer_to_list(Transmission_medium);
		{Key, { dio_out, StationNumber,Index, Time }} ->   STR = atom_to_list(dio_out) ++ "," ++   atom_to_list(StationNumber) ++ "," ++ integer_to_list(Index); 
		{Key, { dio_in, StationNumber,Index, Time }} ->   STR = atom_to_list(dio_in) ++ "," ++  atom_to_list(StationNumber)++ "," ++ integer_to_list(Index);
		{Key, { dao_out, StationNumber,Index, Time } }->   STR = atom_to_list(dao_out) ++ "," ++  atom_to_list(StationNumber)++ "," ++ integer_to_list(Index);
		{Key, { dao_in, StationNumber,Index, Time } }->   STR = atom_to_list(dao_in) ++ "," ++  atom_to_list(StationNumber)++ "," ++ integer_to_list(Index);
		{Key, { data, ReceivingStationNumber, SourceStation, Power, Temperature, Humid, Time } } -> STR = atom_to_list(data) ++ "," ++ atom_to_list(ReceivingStationNumber) ++ "," ++ integer_to_list(SourceStation) ++ "," ++ integer_to_list(Power) ++ "," ++ integer_to_list(Temperature) ++ "," ++ integer_to_list(Humid);
		{Key, { rssi,Fsm_Name,SenderNumber,Index,Transmission_medium,Rssi, Time } }->   STR = atom_to_list(rssi) ++ "," ++  atom_to_list(Fsm_Name) ++ "," ++ atom_to_list(SenderNumber) ++ "," ++ integer_to_list(Index) ++ "," ++ integer_to_list(Transmission_medium) ++ "," ++ integer_to_list(Rssi);
		{Key, { new_Rank, Station, New_Rank, Time } } when is_integer(New_Rank)-> STR = atom_to_list(new_Rank) ++ "," ++  atom_to_list(Station)++ "," ++ integer_to_list(New_Rank);
		{Key, { new_Rank, Station, New_Rank, Time } } when is_atom(New_Rank)-> STR = atom_to_list(new_Rank) ++ "," ++  atom_to_list(Station)++ "," ++ atom_to_list(New_Rank);
		Else -> io:format("db:copoy_db_to_file: ets:lookup got elst:~p~n", [Else]), Time = 0,  STR = ""
	end,
	LOG_STR =  integer_to_list(Key) ++ ")" ++ isg_time:timeStamp_to_Time(Time) ++"," ++ STR ++ "\n",
	_Ans = file:write(FD, LOG_STR),
	%try next log, if fails, try next one
	try
	copy_db_to_file2(DB, ets:next(DB, Key), FD)
	catch
	_:_ -> copy_db_to_file2(DB, ets:next(DB, ets:next(DB, Key) ), FD )
	end.

	

print_db(_,'$end_of_table') -> done;
print_db(DB, Key) -> 
	A = ets:lookup(DB, Key),
	[ {Key, { MSG_Type,  Received_or_Sent,I, Src, Dest,  Channel, RSSI,  _Rest } } ] = A,
	io:format("~p\t\t~p\t\t~p\t\t~p\t\t~p\t\t~p\t\t~p\t\t~p~n", [Key,MSG_Type, Received_or_Sent,I, Src,  Dest, Channel, RSSI]),
	print_db(DB, ets:next(DB, Key)).
	
	
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
	
	
