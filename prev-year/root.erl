%% @author isg-y5-team1
%% @doc @todo Add description to root.

-include("macros.hrl").
-define(Application,app).
-module(root).
-behaviour(gen_fsm).
-export([init/1, active/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([calculatePath/3,start/1]).   %check_func_calculate_path/0,func/3

%check_func_calculate_path() ->
%	NewEts = ets:new(testPath, [named_table]), 
%	ets:insert(NewEts, [{2,4,1},{5,4,2},{6,3,1},{4,3,2},{3,1,1},{7,6,2},{8,1,1},{9,5,1},{10,8,2},{11,2,1},{12,6,1}]),
%	Pathto2 = root:calculatePath(2, NewEts, 1),
%	io:format("the path from root to station 2 is: ~p~n",[Pathto2]),
%	Pathto3 = root:calculatePath(3, NewEts, 1),
%	io:format("the path from root to station 3 is: ~p~n",[Pathto3]),
%	Pathto4 = root:calculatePath(4, NewEts, 1),
%	io:format("the path from root to station 4 is: ~p~n",[Pathto4]),
%	Pathto5 = root:calculatePath(5, NewEts, 1),
%	io:format("the path from root to station 5 is: ~p~n",[Pathto5]),
%	Pathto6 = root:calculatePath(6, NewEts, 1),
%	io:format("the path from root to station 6 is: ~p~n",[Pathto6]),
%	Pathto7 = root:calculatePath(7, NewEts, 1),
%	io:format("the path from root to station 7 is: ~p~n",[Pathto7]),
%	Pathto8 = root:calculatePath(8, NewEts, 1),
%	io:format("the path from root to station 8 is: ~p~n",[Pathto8]),
%	Pathto9 = root:calculatePath(9, NewEts, 1),
%	io:format("the path from root to station 9 is: ~p~n",[Pathto9]),
%	Pathto10 = root:calculatePath(10, NewEts, 1),
%	io:format("the path from root to station 10 is: ~p~n",[Pathto10]),
%	Pathto11 = root:calculatePath(11, NewEts, 1),
%	io:format("the path from root to station 11 is: ~p~n",[Pathto11]),
%	Pathto12 = root:calculatePath(12, NewEts, 1),
%	io:format("the path from root to station 12 is: ~p~n",[Pathto12]),
%	ets:delete(NewEts).

%func(StationPid, StationNumber,Counter) -> 
% 	receive 
% 		stop -> ok
% 	after 
% 		100 -> 
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'3','1',1}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'4','3',2}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'2','4',1}]}),
% 			timer:sleep(900),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'5','4',2}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'6','3',1}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'7','6',2}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['8',StationNumber,{'8','1',1}]}),
% 			timer:sleep(900),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'9','5',1}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['8',StationNumber,{'10','8',2}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'11','2',1}]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {2,['3',StationNumber,{'12','6',1}]}),			
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, print),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'2',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'3',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'4',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'5',[]}),
% 			timer:sleep(50),	
% 			gen_fsm:send_event(StationPid, {send_data_message,'6',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'7',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'8',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'9',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'10',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'11',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, {send_data_message,'12',[]}),
% 			timer:sleep(50),
% 			gen_fsm:send_event(StationPid, print),
% 			if 
% 				Counter > 100 -> func(StationPid, StationNumber,70);
% 				true -> gen_fsm:send_event(StationPid, {1, []}),
% 					timer:sleep(50),
% 					gen_fsm:send_event(StationPid, {1, []}),
% 					func(StationPid, StationNumber,101)
% 			end
% 	end.

-spec start(Args :: atom()) -> FSM_pid :: pid().
start(StationNumber) -> io:format("start func! ~n"),
			{ok,Fsm_pid} = gen_fsm:start_link({local,StationNumber}, ?MODULE, [StationNumber], []),  %{debug,[trace]}
			%Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(Fsm_pid,StationNumber) end),
		 	%register(parseMessageIn,Receiver_process_pid),
			Fsm_pid.
			
%% ====================================================================
%% Behavioural functions
%% ====================================================================
%-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init([StationNumber]) ->
	%Rank = 1,
	VersionNumber = 1,
	NumberOf_Hops = 0,
	process_flag(trap_exit, true),
	EtsTable = ets:new(routing_table, [set,public,named_table,{read_concurrency,true}]),
	Trickle_timer = spawn(fun() -> trickle(StationNumber,StationNumber) end),
	erlang:link(Trickle_timer),
	StateData = [StationNumber,?Root_Rank,VersionNumber,NumberOf_Hops,EtsTable,Trickle_timer],
	Pid = spawn(fun() -> update_ets(EtsTable) end),
	register(etsUpdate,Pid),
	erlang:link(Pid),
	My_pid = self(),
	Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(My_pid,StationNumber) end),
 	register(parseMessageIn,Receiver_process_pid),
	erlang:link(Receiver_process_pid),
	io:format("last line in root::init(). The pid is ~p~n",[My_pid]),
    {ok, active, StateData}.
								  
%% active/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec active(Event :: timeout | term(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
% @todo implement actual state

% incoming DIO message
active({1,_}, StateData) ->
	%io:format("The Root has received an incoming DIO message ~n"), 
	[_StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,_EtsTable,Trickle_timer] = StateData,
	Trickle_timer ! increment,
    {next_state, active , StateData};

% incoming DAO message
active({2, [_SenderNumber,ReceiverNumber,Payload]}, StateData) ->   % ({2, [_SenderNumber,ReceiverNumber,_Tran_Med,Payload]}, StateData)
	%io:format("The Root has received an incoming DAO message ~n"),	
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,
	_MyPid = self(),
	Trickle_timer ! increment,
	{Desc,Desc_parent,Trans_med} = Payload,
	{Ms,Sec,Us} = erlang:now(),
	TimeNow = trunc((Ms*math:pow(10,6) + Sec + Us*math:pow(10,-6))*math:pow(10,3)),
	case ReceiverNumber of
		StationNumber -> 
			%PrevSize = ets:info(EtsTable,size),
			ets:insert(EtsTable, {Desc,Desc_parent,Trans_med,TimeNow}),
			Ans = ets:lookup(EtsTable,Desc),
			case Ans of
				[] -> io:format("The insersion of the record to the ETS failed ~n");
				_ -> io:format("The insertion to the ETS from ~p succedd ~n",[Desc])
			end,
			NewSize = ets:info(EtsTable,size),
			%if
			%	PrevSize =:= NewSize -> ok;
			spawn(fun() -> send_size_route(NewSize) end);
			%end;
		_ -> ok
	end,
	{next_state, active, StateData};

active(timeout_trickle, StateData) ->
	%io:format("Incoming Trickle Event in the root station - sending DIO broadcast message ~n"),
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,_EtsTable,_Trickle_timer] = StateData,
	Receiver = list_to_atom(integer_to_list(16#ff)),
	%io:format("The Receiver number is ~p and its type is an atom?: ~p ~n", [Receiver,is_atom(Receiver)]),
	DIOMessage = {1, [StationNumber,Receiver,_Rank,_VersionNumber,_NumberOf_Hops,3]},
	%DIOMessage = {1, [StationNumber,list_to_atom(integer_to_list(16#ff)),_Rank,_VersionNumber,_NumberOf_Hops,3]},
	hyRPL:outmessage(DIOMessage),%% sending the message to the modem's process!
	{next_state, active, StateData};

active({3,[_SenderNumber,ReceiverNumber,-1,_Payload]}, StateData) -> 
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,_EtsTable,_Trickle_timer] = StateData,	
	case ReceiverNumber of
		StationNumber -> io:format("The Root has recived new data message ~n"), 
				 spawn(fun() -> hyRPL:send_data_stat(StationNumber,_Payload) end),
				 spawn(fun() -> hyRPL:receiver_count_up() end),
				 ?Application:send({data,_Payload}); %% doing something with the payload - processing the message - in the application layer
		_ -> ok
	end,
	{next_state, active, StateData};

active({3, [_SenderNumber,ReceiverNumber,_Path,_Payload]}, StateData) -> 
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,_EtsTable,_Trickle_timer] = StateData,	
	case ReceiverNumber of
		StationNumber -> io:format("Error! the path is cyclic ~n");
		_ -> ok
	end,
	{next_state, active, StateData};

active(increment_version_Number, StateData) ->
	io:format("The root sending a broadcast message to refresh the version-number ~n"),
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,	
	Trickle_timer ! inconsistency,
	NewStateData = [StationNumber,_Rank,_VersionNumber+1,_NumberOf_Hops,EtsTable,Trickle_timer], %increment the version number
	{next_state, active, NewStateData};

active({send_data_message,Destination_station,Payload}, StateData) -> 
	io:format("The Root sends new data message with a specific path ~n"),
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,_Trickle_timer] = StateData,		
	Path_to_destination = calculatePath(Destination_station,EtsTable,StationNumber),
	case Path_to_destination of
		[] -> ok;
		_ -> 
			io:format("The path is ~p~n",[Path_to_destination]),
			Length_of_Path = length(Path_to_destination),

			%%%%% Pattern does not match!
			{NextHop,StationNumber1,Transmission_med,_TimeStamp} = lists:nth(Length_of_Path, Path_to_destination),  % retreive the relevant element from the end of the list
			case StationNumber1 of
				StationNumber -> 
					NewPath = lists:sublist(Path_to_destination, Length_of_Path-1),
					DataMessage = {3, [StationNumber,NextHop,NewPath,Payload], Transmission_med},
					hyRPL:outmessage(DataMessage);%% send the message to the modem's process
				_ -> io:format("Unexpected Path format! ~n")
			end
	end,
	{next_state, active, StateData};	


active(print, StateData) ->
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,	
	ListEts = ets:tab2list(EtsTable),
	io:format("My Fields are: ~p~n ~p~n ~p~n ~p~n ~p~n ~p~n",[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,ListEts,Trickle_timer]),
	{next_state, active, StateData};

active(ets_check, StateData) ->
	io:format("Ets check event ~n"),
	[_StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,_Trickle_timer] = StateData,
	Ets_list = ets:tab2list(EtsTable),
	io:format("The root's ets table is ~p~n",[Ets_list]),
	{next_state, active, StateData};
	
active(stop, StateData) -> 
	%[_StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,	
	%ets:delete(EtsTable),
	%Trickle_timer ! stop,
	%etsUpdate ! stop,
	%parseMessageIn ! stop,
	io:format("The root is terminated ~n"),
	{stop, normal, StateData};

active(_Event, StateData) ->
	io:format("The Root has received unexpected event ~n"),
	{next_state, active, StateData}.


%% state_name/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-3">gen_fsm:StateName/3</a>
-spec state_name(Event :: term(), From :: {pid(), Tag :: term()}, StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: atom(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.


%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
handle_info(Info, StateName, StateData) ->
	[StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,
	MyPid = self(),
	case Info of
		{_,FromPid,_} when is_pid(FromPid) =:= true ->
			case FromPid of
				Trickle_timer -> TrickleTimerPid = spawn(fun() -> trickle(MyPid,StationNumber) end),
						 erlang:link(TrickleTimerPid),
						 NewState = [StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,TrickleTimerPid];
				_ ->
					EtsUpdatePid = whereis(etsUpdate),
					case EtsUpdatePid of
						undefined -> 
							Pid = spawn(fun() -> update_ets(EtsTable) end),
							register(etsUpdate,Pid),
							erlang:link(Pid);
						_ -> ok
					end,
					ParserPid = whereis(parseMessageIn),
					case ParserPid of
						undefined -> % the parser process crushed
							Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(MyPid,StationNumber) end),
							register(parseMessageIn,Receiver_process_pid),
							erlang:link(Receiver_process_pid);
						_ -> ok
					end,
					NewState = StateData
			end;
		_ -> NewState = StateData
	end,
    {next_state, StateName, NewState}.


%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _StateName, StateData) ->
	[_StationNumber,_Rank,_VersionNumber,_NumberOf_Hops,EtsTable,Trickle_timer] = StateData,
	etsUpdate ! stop,	
	ets:delete(EtsTable),
	Trickle_timer ! stop,
	hyRPL:send_message_Fsm(terminate),
    ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% This function calculates the path from the root to a given destination station according to information from the ETS
%% @param Destination_station - the destination station number of the data message
%% @param EtsTable - the name of the ETS table 
%% @param RootNumber - the station numnber of the root
%% @return Path from the root to the destination
-spec calculatePath(Destination_station :: number() ,EtsTable :: atom(), RootNumber :: number()) -> Result :: list().
calculatePath(Destination_station,EtsTable,RootNumber) -> calculatePath(Destination_station,EtsTable,RootNumber,[]).  

calculatePath(Destination_station,EtsTable,RootNumber,Path) -> 
	Station = ets:lookup(EtsTable, Destination_station), 
	case Station of
		[] -> io:format("Error!, there is no path from the root to the given destination ~n"),
			  [];
		_ -> 
			[{Destination_station,Destination_parent,_Transmission_medium,_TimeStamp}] = Station,
			 case Destination_parent of
				 RootNumber -> lists:reverse(Station ++ Path);
		 		 _ -> calculatePath(Destination_parent,EtsTable,RootNumber,Station ++ Path)
	 		 end
	end.

%% A regular timer that after T millisec sends notification to the main process and kills itself
%% @param Pid  - the process id of the main process
%% @param T - time duration of the timer
trickle_timer(Pid,T) -> 
	receive
		{update_timeout,Time} -> trickle_timer(Pid,Time);
		stop -> ok
	after 
		T -> Pid ! timeout
	end.

%% A trickle timer process - envelop function
%% @param Fsm_Pid - the process id of the main process
trickle(Fsm_Pid,Fsm_Name) -> 
	TricklesPid = self(),
	Timer_pid = spawn(fun() -> trickle_timer(TricklesPid,?Imin) end),
	trickle(Fsm_Pid,Fsm_Name,0,?Imin,Timer_pid).

%% A trickle timer process - the main function
%% @param Fsm_Pid - the process id of the main process
%% @param C - the number of management messages that were received in the time duration of the timer
%% @param I - a parameter that moves dynamically from Imin to Imax and the time duration of the timer is determined according to it. 
%% @param Timer_pid - the process id of the timer. 
trickle(Fsm_Pid,Fsm_Name,C,I,Timer_pid) -> 
	%io:format("I is ~p~n",[I]),
	TricklesPid = self(), %% the trickle process id
	receive
		timeout ->  %% the time duraion of the timer is terminated.
				RandomT = random:uniform(),  %% generating random number in order to calculate the next time duration of the timer
				if
					C =< ?K ->  %% sending dio message's notification SHOULD be sent
							gen_fsm:send_event(Fsm_Pid, timeout_trickle),
							if %% updating the value of I
								I < ?Imax -> Inew = 2*I,
											 spawn(fun() -> sendNew_I_Server(Inew,Fsm_Name) end);
								true -> Inew = I
							end, 
							Tnew = round(0.5*(RandomT+1)*Inew), %% calculating the new time duration of the timer
							PidNew = spawn(fun() -> trickle_timer(TricklesPid,Tnew) end), %% initializing the timer with time duration Tnew
							%io:format("I is ~p~n",[Inew]),
							trickle(Fsm_Pid,Fsm_Name,0,Inew,PidNew);
					true ->  %% sending dio message's notification SHOULD NOT be sent
						if 
							I > ?Imin -> Inew = trunc(0.5*I), %% updating the value of I
										%io:format("I is ~p~n",[Inew]),
										 if  %% calculating the new time duration of the timer and initializing the timer with time duration Tnew
											 Inew > ?Imin -> Tnew = round(0.5*(RandomT+1)*Inew),	
										 					 PidNew = spawn(fun() -> trickle_timer(TricklesPid,Tnew) end);
															 %trickle(Fsm_Pid,0,Inew,PidNew);	
											 true -> Tnew = Inew, 
													 PidNew = spawn(fun() -> trickle_timer(TricklesPid,Tnew) end)
													 %trickle(Fsm_Pid,0,Inew,PidNew)
										 end,
										 spawn(fun() -> sendNew_I_Server(Inew,Fsm_Name) end), %% sending the new I to the Local server
										 trickle(Fsm_Pid,Fsm_Name,0,Inew,PidNew);
							true -> Inew = I, %% updating the value of I
									%io:format("I is ~p~n",[Inew]),
									Tnew = I, %% calculating the new time duration of the timer
									PidNew = spawn(fun() -> trickle_timer(TricklesPid,Tnew) end), %% initializing the timer with time duration Tnew
									trickle(Fsm_Pid,Fsm_Name,0,Inew,PidNew)
						end
				end;
		
		inconsistency -> %% the network is unstable, an inconsistency event should reset the timer with time duration Imin
			Timer_pid ! {update_timeout,?Imin},
			Ans = is_process_alive(Timer_pid),
			case Ans of %% checking whether the timer is exist. 
				false -> PidNew = spawn(fun() -> trickle_timer(TricklesPid,?Imin) end);
						 %trickle(Fsm_Pid,0,?Imin,PidNew);
				_ -> PidNew = Timer_pid %trickle(Fsm_Pid,0,?Imin,Timer_pid)
			end,
			spawn(fun() -> sendNew_I_Server(?Imin,Fsm_Name) end),
			spawn(fun() -> send_incons_event_msg({inconsistency, Fsm_Name}) end),
			trickle(Fsm_Pid,Fsm_Name,0,?Imin,PidNew);			

		increment -> %% A management message was received during the time duration of the timer
			%io:format("Trickle increment ~n"),
			trickle(Fsm_Pid,Fsm_Name,C+1,I,Timer_pid);
		
		stop -> io:format("Trickle is terminated ~n"), ok
	end.

%% This function(process function) updates the ETS of the root every constant time - Refresh_time_Ets
%% Each record in the Ets has time-stamp
update_ets(EtsTable) ->
	receive
	  stop -> ok
	after 
		?Refresh_time_Ets ->  %% every minute
			PrevSize = ets:info(EtsTable,size),
			Ets_List = ets:tab2list(EtsTable),
			ets:delete_all_objects(EtsTable),
			{Ms,Sec,Us} = erlang:now(),
			TimeNow = trunc((Ms*math:pow(10,6) + Sec + Us*math:pow(10,-6))*math:pow(10,3)),
			Reduced_List = [{Desc,Desc_parent,Trans_med,Time} || {Desc,Desc_parent,Trans_med,Time} <- Ets_List, TimeNow - Time < ?Record_time_Ets],
			%ets:delete_all_objects(EtsTable),
			try
				[true = ets:insert(EtsTable,[{Key,Key_parent,Med,Time}]) || {Key,Key_parent,Med,Time} <- Reduced_List, ets:lookup(EtsTable,Key) =:= []]
			catch 
				_:Reason -> io:format("Error, the refresh of the Ets went wrong ~p~n",[Reason])
			end,	
			NewSize = ets:info(EtsTable,size),
			%if
			%	PrevSize =:= NewSize -> ok;
			%	true -> 
			spawn(fun() -> send_size_route(NewSize) end),
			%end,
			Check_Ets_Update = ets:tab2list(EtsTable),
			io:format("The new Ets list is: ~p~n",[Check_Ets_Update]),
			gen_fsm:send_event(?Root_Number,ets_check),
			update_ets(EtsTable)
	end.

send_size_route(Size) -> %ok.
	io:format("The Size of the routing table has changed. The new size is ~p ~n", [Size]),
	?LocalServer:send({source_route_tabSize, Size}).
	
sendNew_I_Server(Inew,Fsm_Name) -> %ok.
	io:format("The new I of the trickle in station ~p is ~p ~n",[Fsm_Name,Inew]),
	?LocalServer:send({newI,Inew,Fsm_Name}).

send_incons_event_msg({inconsistency,Fsm_Name}) -> %ok.
	io:format("inconsistency event of StationNumber ~p ~n", [Fsm_Name]),
	?LocalServer:send({inconsistency,Fsm_Name}).	
	
%% qlc:q(  ets:table(EtsTable)	
