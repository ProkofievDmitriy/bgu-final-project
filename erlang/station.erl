%% @author isg-y5-team1
%% @doc @todo Add description to station.

-include("macros.hrl").
-module(station).
-behaviour(gen_fsm).
-export([init/1, non_active/2, active/2, dio_block/2, disconnect/2, state_name/3, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% ====================================================================
%% API functions
%%
%% ====================================================================
-export([start/1, trickle/2, sort/2, swap/1]).  %,outmessage/1, incoming_message/1

%% This function creates the FSM and the process which suppose to create receive the incoming messages from the medium
%% @param Station number - an atom - the local registered name of the FSM
%% @return the FSM pid.
-spec start(Args :: atom()) -> FSM_pid :: pid().
start(StationNumber) -> io:format("start func! ~n"),
						{ok,Fsm_pid} = gen_fsm:start_link({local,StationNumber}, ?MODULE, [StationNumber], []),   %% {debug,[trace]}
						%Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(Fsm_pid,StationNumber) end),
						%register(parseMessageIn,Receiver_process_pid),  %% should be locally registered in order to send it messages without knowing its pid
						Fsm_pid.
	
swap(FileName) ->
	io:format(standard_error,"swap function! ~n",[]),
	Current_file = atom_to_list(?MODULE)++".erl",
	file:delete(Current_file),
	%timer:sleep(300),
	io:format(standard_error,"old module deleted! ~n",[]),
	file:rename(FileName,Current_file),
	%timer:sleep(300),
	io:format(standard_error,"file's name was changed! ~n",[]),
	compile:file(?MODULE),
	sys:suspend(?MODULE),
	code:purge(?MODULE),
	code:load_file(?MODULE),
	sys:change_code(?MODULE,?MODULE,"0",[]),
	io:format("resume the server ~n"),
	sys:resume(?MODULE).

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
	%OutputPid = spawn(fun() -> bar:start() end), %% should be updated later!
	%?ModemModule:start(StationNumber),
	%io:format("init func! ~n"),
	Rank = infinity,
	VersionNumber = -1,
	NumberOfHops = infinity,
	Parent_Set = [],
	DaoMessageList = [],
	%io:format("init 2! ~n"),
	Trickle_timer = nil,
	Refresh_parent_set_timer = nil,
	DAO_flag = 0,
	DAO_notify_timer = nil,
	My_pid = self(),
	process_flag(trap_exit, true),
	io:format("init - last command in creation of the FSM id: ~p in function init - move to state: non_active ~n",[My_pid]),
	Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(My_pid,StationNumber) end),
	register(parseMessageIn,Receiver_process_pid),
	erlang:link(Receiver_process_pid),
    {ok, non_active, [StationNumber,Parent_Set,DaoMessageList,Rank,VersionNumber,NumberOfHops,DAO_flag,DAO_notify_timer,Refresh_parent_set_timer,Trickle_timer]}.


%% non_active/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec non_active(Event :: timeout | term(), StateData :: term()) -> Result when
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
non_active({1,[SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]}, StateData) ->
	case Rank of
		infinity -> %io:format("Incoming Dio message - state: non_active! Rank: infinity ~n"),
					{next_state, non_active, StateData};
		_ ->
			%io:format("Incoming Dio message - state: non_active! Rank: ~p~n",[Rank]),
			[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
			Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
			My_Rank = Rank + Link_cost,
			MyPid = self(),
			{MegaSec,Sec,MicroSec} = erlang:now(),
			TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),
			TrickleTimerPid = spawn(fun() -> trickle(MyPid,StationNumber) end),
			erlang:link(TrickleTimerPid),
			DAO_notifyTimerPid = spawn(fun() -> regular_timer(MyPid,?MY_DAO_DELAY,my_dao) end),
			erlang:link(DAO_notifyTimerPid),
			Refresh_ParentSet_TimerPid = spawn(fun() -> parents_update_timer(MyPid,TimeNow) end),
			erlang:link(Refresh_ParentSet_TimerPid),
			spawn(fun() -> send_new_Pref_Parent(SenderNumber,StationNumber,Transmission_medium) end),
			spawn(fun() -> send_new_Rank(StationNumber,My_Rank) end),
			{next_state, dio_block , [StationNumber,[{{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,My_Rank}],[],My_Rank, VersionNumber,NumberOfHops+1,1,DAO_notifyTimerPid,Refresh_ParentSet_TimerPid,TrickleTimerPid]}
	end;

non_active(print,StateData) -> 
 	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	io:format("My fields are: StationNumber: ~p~n, Parent_Set: ~p~n, Dao Message List: ~p~n, _Rank: ~p~n, VersionNumber: ~p~n, Number Of Hops: ~p~n, DAO_flag: ~p~n, DAO_notify_timer: ~p~n, _Refresh_parent_set_timer: ~p~n, _Trickle_timer: ~p~n",[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]),
	{next_state, non_active, StateData};

non_active(stop, StateData) ->
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	%parseMessageIn ! stop,
	io:format("The station: ~p is terminated ~n",[StationNumber]),
	{stop, normal, StateData};	

% event-handler for all other unexpected incoming messages
non_active(_Event,StateData) -> 
	io:format("In non-active all state ~n"),
	{next_state, non_active, StateData}.
%% active/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/mahttps://bgu4u.bgu.ac.il/pls/scwp/!sc.AnnualCoursesAdvn/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
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
% incoming timeout message from the trickle - send DIO message
active(timeout_trickle, StateData) ->
	%io:format("Incoming internal Event from the trickle! - sending DIO message ~n"),
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	DioMessage = {1, [_StationNumber, list_to_atom(integer_to_list(16#ff)), _Rank, _VersionNumber,_NumberOfHops, 3]}, % send a broadcast dio message
	hyRPL:outmessage(DioMessage),  %% sending the message to bar's process!
	case _DAO_flag of 
		1 ->
			io:format("DIO flag is high - sending DAO message as well ~n"),
			{{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_SINR,_Bitrate,_TimeNow,_My_Rank} = lists:nth(1, _Parent_Set),
			Payload = {_StationNumber,SenderNumber,Transmission_medium},
			DaoMessage = {2, [_StationNumber,SenderNumber,Transmission_medium,Payload]},
			hyRPL:outmessage(DaoMessage),%% sending the message to bar's process!
			NewStateData = [_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,0,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
			{next_state, active, NewStateData};
		_ -> {next_state, active , StateData}
	end;

% incoming timeout message - send DAO message
active({timeout,my_dao}, StateData) ->
	%io:format("Incoming internal event - sending my own DAO message ~n"),
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case _DAO_flag of 
		1 -> io:format("DIO flag is high, state: active, the sending is suspended ~n"), ok;
		_ -> 
			{{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_SINR,_Bitrate,_TimeNow,_My_Rank} = lists:nth(1, _Parent_Set),
			Payload = {_StationNumber,SenderNumber,Transmission_medium},
			DaoMessage = {2, [_StationNumber,SenderNumber,Transmission_medium,Payload]},
			hyRPL:outmessage(DaoMessage) %% sending the message to bar's process!
	end,
    {next_state, active , StateData};																																				

% incoming DIO message
active({1,[SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]}, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	{MegaSec,Sec,MicroSec} = erlang:now(),
	TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)), %% time in millisec
	MyPid = self(),
	%io:format("incoming DIO message from ~p~n", [SenderNumber]),
	if %% version number's check
		VersionNumber < _VersionNumber -> %io:format("The Version Number is lower ~n"),
											_Trickle_timer ! increment,
										  {next_state, active , StateData};
		VersionNumber > _VersionNumber -> 	%% reset the state of the station
											%io:format("The Version Number is higher ~n"),
										case Rank of
											infinity -> io:format("New Incoming DIO message with higher version number but from a station with rank infinity. The message is ignored ~n"),
											    {next_state,active,StateData};
											_ -> io:format("******************************************** Version-Number increased ***********************************~n"),
											     io:format("***************************************************************************************************************************************************************** ~n ******************************************************************************************************************************** ~n *************************************************************************************************************** ~n"),
												Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
												MyRank = Rank + Link_cost,
												_Trickle_timer ! inconsistency,
												_Refresh_parent_set_timer ! {reset_timer,TimeNow},
												IsAlive = is_process_alive(_Refresh_parent_set_timer),
												case IsAlive of 
													true -> NewRefresh_TimePid = _Refresh_parent_set_timer;
													false -> NewRefresh_TimePid = spawn(fun() -> parents_update_timer(MyPid, TimeNow) end),
														 erlang:link(NewRefresh_TimePid)
												end,
								%% ******************************************************************** The Version Number should be updated!!! change _VersionNumber to VersionNumber in the next line *****************************************
												NewStateData = [_StationNumber,[{{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank}],[],MyRank,VersionNumber,NumberOfHops+1,1,_DAO_notify_timer,NewRefresh_TimePid,_Trickle_timer],
												spawn(fun() -> send_new_Pref_Parent(SenderNumber,_StationNumber,Transmission_medium) end),
												spawn(fun() -> send_new_Rank(_StationNumber,MyRank) end),
												{next_state, dio_block, NewStateData}
										end;
		true -> 
			%io:format("Dio message with the same version number ~n"),
			if %% check if the rank of the sender is higher or lower than mine,
				Rank =:= infinity -> %io:format("infinity rank dio message ~n"),%_Trickle_timer ! increment,
								 %{next_state, active , StateData};
					IsParentMember = [{PSenderNumber,PTransmission_medium} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_,_,_,_} <- _Parent_Set, PSenderNumber =:= SenderNumber],
					case IsParentMember of
						[] -> %io:format("incoming DIO message from a station with rank infinity - the parent is not in the parent list ~n"),
							_Trickle_timer ! increment,
							 {next_state, active , StateData};
						_ -> 
							Pred = fun({{PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_TimeNow,_MyRank}) -> PSenderNumber /= SenderNumber end, %% predicat that filter all the records from the given parent
							NewParentSet = lists:filter(Pred, _Parent_Set),
							case NewParentSet of
								[] -> %io:format("incoming DIO message from a station with rank infinity - the parent is in the parent list, the new parent list is empty! ~n"),
									 _Trickle_timer ! inconsistency,
									  MyNewRank = infinity,
									  _Refresh_parent_set_timer ! stop,
									  _DAO_notify_timer ! stop,
									  %% list compr. for stop the timers
									  NewStateData = [_StationNumber,NewParentSet,[],MyNewRank,_VersionNumber,infinity,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									  spawn(fun() -> send_new_Pref_Parent(none,_StationNumber,none) end),
									  spawn(fun() -> send_new_Rank(_StationNumber,infinity) end),
									  {next_state, disconnect, NewStateData};
								_ -> %% the new parent set is not empty
									CompareHead = headMatch(_Parent_Set, NewParentSet), %% checks match according to my relative rank via those parents!!!
									CompareTail = tailMatch(_Parent_Set, NewParentSet), %% checks match according to parent's update time!!!
									case CompareHead of
										true -> 
											_Trickle_timer ! increment,
											%io:format("incoming DIO message from a station with rank infinity - the parent is in the parent list, the new parent list is not empty, the same preferred parent ~n"),
											case CompareTail of 
												true ->  %% the same old parent
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												{false, RefreshTime} -> %% new old parent, timer-refresh parent should be reset
													%io:format("Refresh parent set timer ~n"),
													_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
													IsAlive = is_process_alive(_Refresh_parent_set_timer),
													case IsAlive of
														true -> %% in case the timer is still alive
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
														false -> %% in case the timer is already dead until the refresh message has received
															Newtimer_ParentSet = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
															erlang:link(Newtimer_ParentSet),
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,Newtimer_ParentSet,_Trickle_timer]
													end
											end,
											{next_state, active, NewStateData};
										false ->
											_Trickle_timer ! inconsistency,
											%io:format("incoming DIO message from a station with rank infinity - the parent is in the parent list, the new parent list is not empty, new preferred parent! ~n"),
											{{_PSenderNumber1,_PTransmission_medium1},_PRank1,_PVersionNumber1,_PNumberOfHops1,_SINR1,_Bitrate1,_TimeNow1,_My_Rank1} = lists:nth(1, NewParentSet),
											case CompareTail of
												true -> 
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												{false, RefreshTime} -> 												
													_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
													IsAlive = is_process_alive(_Refresh_parent_set_timer),
													case IsAlive of
														true ->
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
														false -> 
															Newtimer_ParentSet = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
															erlang:link(Newtimer_ParentSet),
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,Newtimer_ParentSet,_Trickle_timer]
													end
											end,
											spawn(fun() -> send_new_Pref_Parent(_PSenderNumber1,_StationNumber,_PTransmission_medium1) end),
											spawn(fun() -> send_new_Rank(_StationNumber,_My_Rank1) end),
											{next_state, dio_block, NewStateData}
									end
							end
					end;
											
				Rank >= _Rank ->
									%io:format("incoming DIO message from a station with rank higher than mine! ~n"),
									_Trickle_timer ! increment,
									{next_state, active , StateData};
				true -> 
					%io:format("The same Version Number ~n"),
					IsParentMember = [{PSenderNumber,PTransmission_medium,RRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_,_,_,RRank} <- _Parent_Set, PSenderNumber =:= SenderNumber, PTransmission_medium =:= Transmission_medium],
					case IsParentMember of   %% check if the sender is a potential parent
						[] -> %io:format("New Parent!, state: active ~n"), %% the sender is not in my parents' set	
							Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
							MyRank = Rank + Link_cost,
							Parent = {{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank},
							%NewParentSet = sort([Parent]++_Parent_Set),
							NewParentSet = sort(_Parent_Set,Parent),
							{{Parent_Name,Trans_Parent},_,_,_PNumberOfHops,_,_,_,_PMy_Rank} = lists:nth(1, NewParentSet),  
							%LeastPotential_Parent = lists:nth(length(_Parent_Set),_Parent_Set),
							%NewLeastPotentialParent = lists:nth(length(NewParentSet), NewParentSet),
							%NewPreferred_Parent = lists:nth(1,NewParentSet),
							%Preferred_Parent = lists:nth(1,_Parent_Set),
							CompareHead = headMatch(_Parent_Set,NewParentSet), %% checks match according to my relative rank via those parents!!!
							%CompareTail = tailMatch(_Parent_Set,NewParentSet), %% checks match according to parent's update time!!!
							case CompareHead of		%% check if my preferred parent was changed
								true -> 
									_Trickle_timer ! increment,
									NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_PMy_Rank,VersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									spawn(fun() -> send_new_Rank(_StationNumber,_PMy_Rank) end),
%									case CompareTail of %%  check if the worst potential parent was changed
%										true -> ok;
%										{false, RefreshTime} ->
%											_Refresh_parent_set_timer ! {reset_timer,RefreshTime}
%									end,
									{next_state, active, NewStateData};
								false -> 
									_Trickle_timer ! inconsistency,
									NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,MyRank,VersionNumber,_PNumberOfHops+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									spawn(fun() -> send_new_Pref_Parent(Parent_Name,_StationNumber,Trans_Parent) end),
									spawn(fun() -> send_new_Rank(_StationNumber,MyRank) end),
									{next_state, dio_block, NewStateData}
							end;
						[{_,_,RR}] -> 
							%io:format("Existing parent! state: active ~n"), %% the sender is in my parents' set
							Pred = fun({{PSenderID,PTransmissionMedium},_PRank1,_PVR,_Pnoh,_PSINR,_PBITRATE,_PTN,_PMR}) -> {PSenderID,PTransmissionMedium} /= {SenderNumber,Transmission_medium} end, %% predicat that filters the potential parent with the given keys 
							ReducedParentSet = lists:filter(Pred, _Parent_Set),
							%io:format("the reduce set is ~p~n", [ReduceSet]),
							%ReducedParentSet = [{{PSenderNumber,PTransmission_medium},PRank1,PVersionNumber1,PNumberOfHops1,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},PRank1,PVersionNumber1,PNumberOfHops1,PSINR,PBitrate,PTimeNow,PMyRank} <- _Parent_Set, PSenderNumber /= SenderNumber, PTransmission_medium /= Transmission_medium],
							%io:format("the former parent set is: ~p~n, the reduced parent set is: ~p~n", [_Parent_Set,ReducedParentSet]),
							Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
							MyRank_tmp = Rank + Link_cost,
							MyRank = round(?Alpha*MyRank_tmp + (1-?Alpha)*RR),
							Parent = {{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank},
							%NewParentSet = sort([Parent]++ReducedParentSet),
							NewParentSet = sort(ReducedParentSet,Parent),
							%io:format("the new parent set is: ~p~n",[NewParentSet]),
							CompareHead = headMatch(_Parent_Set,NewParentSet), %% checks match according to my relative rank via those parents!!!
							CompareTail = tailMatch(_Parent_Set,NewParentSet), %% checks match according to parent's update time!!!
							{{Parent_Name,Trans_Parent},_,_PVersionNumber,_PNumberOfHops,_,_,_,PMyRank} = lists:nth(1, NewParentSet),
							spawn(fun() -> send_new_Rank(_StationNumber,PMyRank) end),
							case CompareHead of 
								true -> 
									%io:format("The same preferred parent after change ~n"),
									if 
										PMyRank /= _Rank -> %io:format("My Rank has changed! ~n"),
															DioBlockFlag = 1,
															_Trickle_timer ! inconsistency; %% if the station's rank has changed but my preferred parent has not changed, we would like
																							%% to send broadcast message with the minimum delay
										true ->	DioBlockFlag = 0,
												_Trickle_timer ! increment
									end,
									case CompareTail of
										true -> 
											NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
										{false, RefreshTime} -> 
											_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
											IsAlive = is_process_alive(_Refresh_parent_set_timer),
											case IsAlive of
												true -> %% in case the refresh parent timer is still alive
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												false ->  %% in case the timer is already dead before the message has received
													New_Parents_refresh_timerPid = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
													erlang:link(New_Parents_refresh_timerPid),
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,New_Parents_refresh_timerPid,_Trickle_timer]
											end
									end,
									case DioBlockFlag of
										1 -> {next_state, dio_block , NewStateData};
										_ -> {next_state, active, NewStateData}
									end;  %% finish - true
								false -> 
									%io:format("New preferred parent! ~n"),
									case CompareTail of
										true -> 
											NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
										{false, RefreshTime} -> _Refresh_parent_set_timer ! {reset_timer,RefreshTime},
																IsAlive = is_process_alive(_Refresh_parent_set_timer),
																case IsAlive of
																	true -> %% in case the refresh parent timer is still alive
																		NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
																	false -> %% in case the timer is already dead before the message has received
																		New_Parents_refresh_timerPid = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
																		erlang:link(New_Parents_refresh_timerPid),
																		NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,New_Parents_refresh_timerPid,_Trickle_timer]
																end																
									end,
									_Trickle_timer ! inconsistency,
									spawn(fun() -> send_new_Pref_Parent(Parent_Name,_StationNumber,Trans_Parent) end),
									{next_state, dio_block, NewStateData}
							end; %% finish false and CompareHead
					      _ ->  io:format("Unexpected case - incoming DIO message for a station with lower rank that exists more than once in the parent set ~n"),
						{next_state,active,StateData}
					end
			end
	end;	

active(print,StateData) ->  %% this Event-handler prints the state-data of the station 
 	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	io:format("My fields are: StationNumber: ~p~n, Parent_Set: ~p~n, Dao Message List: ~p~n, _Rank: ~p~n, VersionNumber: ~p~n, Number Of Hops: ~p~n, DAO_flag: ~p~n, DAO_notify_timer: ~p~n, _Refresh_parent_set_timer: ~p~n, _Trickle_timer: ~p~n",[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]),
	{next_state, active , StateData};


active(stop, StateData) -> %% this Event-handler kills the FSM
	io:format("FSM terminated event has received ~n"),
	 [_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	 %_DAO_notify_timer ! stop, 
	 %_Refresh_parent_set_timer ! stop,
	 %_Trickle_timer ! stop,
	 %parseMessageIn ! stop,
	 io:format("Station ~p is terminated ~n",[_StationNumber]),
	{stop, normal, StateData};

% incoming timeout - time to forward DAO message from the list of DAO messages
active({forwardDAO_timeout,Desc,TimerPid}, StateData) ->
	%io:format("Incoming internal event - forward DAO message from the list of the given Descendant ~n"),
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case DaoMessageList of
		[] -> {next_state, active, StateData};
		_ -> Pred = fun({DaoPayload,_TimerPid}) -> DaoPayload =:= Desc andalso _TimerPid =:= TimerPid end, %% predicat that filters a record according to the parameters in this event
			 IsExist = lists:filter(Pred, DaoMessageList),
			 case IsExist of
				 [] -> {next_state, active, StateData};
				 _ ->
					 NewDaoMessageList = DaoMessageList -- IsExist,
					 {{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1, _Parent_Set),
					 DaoMessage = {2,[StationNumber,SenderNumber,Transmission_medium,Desc]},
					 hyRPL:outmessage(DaoMessage),%% send the message to Bar's process!!!
					 NewStateData = [StationNumber,_Parent_Set,NewDaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
					 {next_state, active, NewStateData}
			 end
	end;

% incoming timeout message - updating my parents set
active({refresh_parents_list, TimeNow}, StateData) ->
	%io:format("Refreshing parents' list! ~n"),
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	%{MegaSec,Sec,MicroSec} = erlang:now(),
	%TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),							
	Pred = fun({{_SenderNumber,_Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank}) -> TimeNow - _PTimeNow < ?ConstantDelayParent end, %% predicat that filter all the old potential parents - in the parent set more than the constant-delay time
	NewParentSet = lists:filter(Pred, _Parent_Set),
	%io:format("the new parent set is ~p~n",[NewParentSet]),
	case NewParentSet of
		[] ->
			%io:format("ParentSet IS empty! move to state: disconnect~n"),
			_DAO_notify_timer ! stop,
			_Trickle_timer ! inconsistency,
			NewStateData = [StationNumber,NewParentSet,[],infinity,_VersionNumber,infinity,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
			spawn(fun() -> send_new_Pref_Parent(none,StationNumber,none) end),
			spawn(fun() -> send_new_Rank(StationNumber,infinity) end),
			{next_state, disconnect, NewStateData}; %% parent set is empty
		_ -> 
			%io:format("ParentSet is not empty ~n"),
			{_,ParentUpdateTime} = findOldestParent(NewParentSet),
			MyPid = self(),
			%io:format("My pid is: ~p, The oldest parent time is: ~p~n",[MyPid,ParentUpdateTime]),
			New_ParentSet_timerPid = spawn(fun() -> parents_update_timer(MyPid,ParentUpdateTime) end),
			erlang:link(New_ParentSet_timerPid),
			%io:format("New timer started! The pid is ~p~n", [New_ParentSet_timerPid]),
			CompareHead = headMatch(_Parent_Set,NewParentSet), %% checks match according to my relative rank via those parents!!!
			case CompareHead of
				true -> 
					NewStateData = [StationNumber,NewParentSet,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,New_ParentSet_timerPid,_Trickle_timer],
					spawn(fun() -> send_new_Rank(StationNumber,_Rank) end),
					{next_state, active, NewStateData};
				false ->
					%io:format("new preferred parent! move to state: dio_block ~n"),
					_Trickle_timer ! inconsistency,
					{{_PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1, NewParentSet),	
					NewStateData = [StationNumber,NewParentSet,DaoMessageList,_PMyRank,_VersionNumber,_PNumberOfHops+1,1,_DAO_notify_timer,New_ParentSet_timerPid,_Trickle_timer],
					spawn(fun() -> send_new_Pref_Parent(_PSenderNumber,StationNumber,_PTransmission_medium) end),
					spawn(fun() -> send_new_Rank(StationNumber,_PMyRank) end),
					{next_state, dio_block , NewStateData}
			end
	end;

% incoming DAO message
active({2, [_SenderNumber,ReceiverNumber,Payload]}, StateData) ->
	%io:format("New DAO message has received! ~n"),
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	{Descendant,Descendant_parent,_Transmission_medium} = Payload,
	MyPid = self(),
	_Trickle_timer ! increment,
	case ReceiverNumber of
		StationNumber -> % the message was sent to me - unicast!
			Pred = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station =:= Descendant end, %% predicat to retrieve all the records with the key = Descendant in the DAO message list 
			Pred2 = fun({{Station1,StationParent1,_Transmission_med1},_TimerPid1}) -> Station1 =:= Descendant andalso (StationParent1 /= Descendant_parent orelse _Transmission_med1 /= _Transmission_medium) end, %% predicat to retrieve avoid multiple records with the same key (Descendant)
			Dao_Former_Messages = lists:filter(Pred, DaoMessageList), %% using the first predicat on the original DAO message list
			DAO_listMessages = lists:filter(Pred2, Dao_Former_Messages), %% using the second predicat on the reduced DAO message list - Dao_Former_Messages
			DaoMessage = Dao_Former_Messages -- DAO_listMessages, %% the only possible relevant DAO message
			List = DaoMessageList -- DAO_listMessages, %% the reduced new DAO message list
			case DaoMessage of
				[] -> %% The relevant DAO message list is exist yet
					  NewTimerPid = spawn(fun() -> timer_forward_dao(MyPid,Payload) end), %% a new timer is created so after a short delay this message will be forwarded through the root
					  NewDao_MessageList = [{Payload,NewTimerPid}] ++ List,
					  NewStateData = [StationNumber,_Parent_Set,NewDao_MessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];		  
				_ -> %% The relevant DAO message exist in the DAO message list!
					 [{_Payload1,TimerPid1}] = DaoMessage,
					 TimerPid1 ! reset,
					 IsAlive = is_process_alive(TimerPid1),
					 case IsAlive of
						 true -> %the timer is still alive
							 NewDao_MessageList = List,
							 NewStateData = [StationNumber,_Parent_Set,NewDao_MessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
						 false -> %% the timer is no longer alive, a new record of this message should be created in the DAO message list
						 		  NewList = List -- DaoMessage,
								  NewTimerPid = spawn(fun() -> timer_forward_dao(MyPid,Payload) end),
								  NewDao_MSG_List = [{Payload,NewTimerPid}]++NewList,
								  NewStateData = [StationNumber,_Parent_Set,NewDao_MSG_List,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]
					 end
			end,
			{next_state, active, NewStateData};		 
		
		_ -> %% in case of incoming DAO message which is not addressed to me
			%io:format("New incoming message which is not addressed to me! ~n"), 
			Pred = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station =:= Descendant end, %% predicat that filters all the records with the key = Descendant
			Deleted_DAO_messages = lists:filter(Pred, DaoMessageList),
			case Deleted_DAO_messages of
				[] -> %% the message is not exist in my DAO message list
					NewStateData = StateData;
				_ ->  %% the message from the given Descendant do exist
					[TimerPid ! stop || {_Payload,TimerPid} <- Deleted_DAO_messages],
					Pred2 = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station /= Descendant end, %% predicat that filters all the records with the key = Descendant
					NewDao_MSG_List = lists:filter(Pred2, DaoMessageList),
					NewStateData = [StationNumber,_Parent_Set,NewDao_MSG_List,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]
			end,
			{next_state, active, NewStateData}
	end;

% incoming data message without a source-route path
active({3,[_SenderNumber,ReceiverNumber,-1,Payload]}, StateData) -> 
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case ReceiverNumber of
		StationNumber ->
			spawn(fun() -> hyRPL:send_data_stat(StationNumber,Payload) end), 
			{{PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1,_Parent_Set),
			DataMessage = {3, [StationNumber,PSenderNumber,-1,Payload], _PTransmission_medium},
			case _SenderNumber of
				'1' -> spawn(fun() -> hyRPL:up_data_counter_origen() end);
				_ -> ok
			end,
			hyRPL:outmessage(DataMessage);%% send the message to bar's process via the best transmission medium between the Station and its preferred parent: the last field in the Data message - should be discarded by bar's process

%		16#ffffffff -> ok; % incoming broadcast data messages - in the application layer
		_ -> ok
	end,
	{next_state, active, StateData};

% incoming data message with a source-route path 
active({3, [_SenderNumber, ReceiverNumber, Path, Payload]}, StateData) when is_list(Path) =:= true->  %% path structure created by the root: 
																		  %% [Destination, MidStation_N, MidStation_N-1,...,MidStation_1,root]
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case ReceiverNumber of
		StationNumber -> 
			spawn(fun() -> hyRPL:send_data_stat(StationNumber,Payload) end), 
			Path_length = length(Path),
			case Path_length of
				0 -> ok; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   1 -> ok; 
					 %% doing something with the payload - processing the message - in the application layer
				_ -> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% try-catch
					{NewReceiver,StationNumber1,Transmission_medium} = lists:nth(Path_length, Path), 
					if
						StationNumber1 =:= StationNumber -> 
							NewPath = lists:sublist(Path, Path_length-1),
							DataMessage = {3, [StationNumber,NewReceiver,NewPath,Payload], Transmission_medium},
							hyRPL:outmessage(DataMessage);%% send the message to the modem's process - the last field in the Data message is 'Transmission_medium' and should be discarded by the modem's process
						true -> io:format("Unexpected Path format! ~n")
					end	
			end;

%		16#ffffffff -> ok; % incoming broadcast data messages - in the application layer
		_ -> ok
	end,
	{next_state, active, StateData};

% event handler for all other unexpected incoming messages
active(_Event,StateData) ->
	io:format("Unexpected message ~p~n",[_Event]),
	{next_state, active, StateData}.


%% dio_block/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec dio_block(Event :: timeout | term(), StateData :: term()) -> Result when
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

% incoming timeout from the trickle process
dio_block(timeout_trickle, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	DioMessage = {1, [_StationNumber, list_to_atom(integer_to_list(16#ff)), _Rank, _VersionNumber,_NumberOfHops, 3]}, % send a broadcast dio message
	hyRPL:outmessage(DioMessage),%% sending the message to bar's process!
	case _DAO_flag of 
		1 ->
			{{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_SINR,_Bitrate,_TimeNow,_My_Rank} = lists:nth(1, _Parent_Set),
			Payload = {_StationNumber,SenderNumber, Transmission_medium},
			DaoMessage = {2, [_StationNumber,SenderNumber,Transmission_medium,Payload]},
			hyRPL:outmessage(DaoMessage),%% sending the message to bar's process!
			NewStateData = [_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,0,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
			{next_state, active, NewStateData};
		_ -> {next_state, active , StateData}
	end;

% incoming DIO message
dio_block({1,[SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]}, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	{MegaSec,Sec,MicroSec} = erlang:now(),
	TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),
	MyPid = self(),
	if %% version number's check
		VersionNumber < _VersionNumber -> %io:format("The Version Number is lower ~n"),
											_Trickle_timer ! increment,
										  {next_state, dio_block , StateData};
		VersionNumber > _VersionNumber -> 	%io:format("The Version Number is higher ~n"),
										case Rank of
											infinity -> io:format("New incoming DIO message with higher version number but from a station with rank infinity~n"),
											    {next_state,dio_block,StateData};
											_ -> io:format("******************************************** Version-Number increased ***********************************~n"),
											     io:format("***************************************************************************************************************************************************************** ~n ************************************************************************************************************************************* ~n ************************************************************************************************************************************* ~n"),
												Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
												MyRank = Rank + Link_cost,
												_Trickle_timer ! inconsistency,
												_Refresh_parent_set_timer ! {reset_timer,TimeNow},
												IsAlive = is_process_alive(_Refresh_parent_set_timer),
												case IsAlive of 
													true -> NewRefresh_TimePid = _Refresh_parent_set_timer;
													false -> NewRefresh_TimePid = spawn(fun() -> parents_update_timer(MyPid, TimeNow) end),
														 erlang:link(NewRefresh_TimePid)
												end,
								%% ******************************************************************** The Version Number should be updated!!! change _VersionNumber to VersionNumber in the next line *****************************************
												NewStateData = [_StationNumber,[{{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank}],[],MyRank,VersionNumber,NumberOfHops+1,1,_DAO_notify_timer,NewRefresh_TimePid,_Trickle_timer],
												spawn(fun() -> send_new_Pref_Parent(SenderNumber,_StationNumber,Transmission_medium) end),
												spawn(fun() -> send_new_Rank(_StationNumber,MyRank) end),
												{next_state, dio_block, NewStateData}
										end;
		true -> %% the same version number as mine
			if %% check if the rank of the sender is higher or lower than mine,
				Rank =:= infinity -> 
					IsParentMember = [{PSenderNumber,PTransmission_medium} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_,_,_,_} <- _Parent_Set, PSenderNumber =:= SenderNumber],
					case IsParentMember of
						[] -> _Trickle_timer ! increment,
							 {next_state, dio_block , StateData};
						_ -> 
							Pred = fun({{PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_TimeNow,_MyRank}) -> PSenderNumber /= SenderNumber end,
							NewParentSet = lists:filter(Pred, _Parent_Set),
							case NewParentSet of
								[] -> _Trickle_timer ! inconsistency,
									  MyNewRank = infinity,
									  _Refresh_parent_set_timer ! stop,
									  _DAO_notify_timer ! stop,
									  NewDAO_MessageList = [], %% list compr. for stop the timers
									  NewNumberOfHops = infinity,
									  NewStateData = [_StationNumber,NewParentSet,NewDAO_MessageList,MyNewRank,_VersionNumber,NewNumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									  spawn(fun() -> send_new_Pref_Parent(none,_StationNumber,none) end),
									  spawn(fun() -> send_new_Rank(_StationNumber,infinity) end),
									  {next_state, disconnect, NewStateData};
								_ -> 
									CompareHead = headMatch(_Parent_Set, NewParentSet), %
									CompareTail = tailMatch(_Parent_Set, NewParentSet), %% checks match according to parent's update time!!!
									case CompareHead of
										true -> 
											_Trickle_timer ! increment,
											case CompareTail of 
												true -> 
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												{false, RefreshTime} -> 
													_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
													IsAlive = is_process_alive(_Refresh_parent_set_timer),
													case IsAlive of
														true ->
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
														false -> 
															Newtimer_ParentSet = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
															erlang:link(Newtimer_ParentSet),
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,Newtimer_ParentSet,_Trickle_timer]
													end
											end,
											{next_state, dio_block , NewStateData};
										false -> 
											_Trickle_timer ! inconsistency,
											{{_PSenderNumber1,_PTransmission_medium1},_PRank1,_PVersionNumber1,_PNumberOfHops1,_SINR1,_Bitrate1,_TimeNow1,_My_Rank1} = lists:nth(1, NewParentSet),
											case CompareTail of
												true -> 
													NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												{false, RefreshTime} -> 												
													_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
													IsAlive = is_process_alive(_Refresh_parent_set_timer),
													case IsAlive of
														true ->
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
														false -> 
															Newtimer_ParentSet = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
															erlang:link(Newtimer_ParentSet),
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_My_Rank1,_VersionNumber,_PNumberOfHops1+1,1,_DAO_notify_timer,Newtimer_ParentSet,_Trickle_timer]
													end
											end,
											spawn(fun() -> send_new_Pref_Parent(_PSenderNumber1,_StationNumber,_PTransmission_medium1) end),
											spawn(fun() -> send_new_Rank(_StationNumber,_My_Rank1) end),
											{next_state, dio_block, NewStateData}
									end
							end
					end;
											
				Rank >= _Rank ->   %% ignore the message
						_Trickle_timer ! increment,
						{next_state, dio_block , StateData};
				true -> 
					%io:format("The same Version Number ~n"),
					IsParentMember = [{PSenderNumber,PTransmission_medium,RRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_,_,_,RRank} <- _Parent_Set, PSenderNumber =:= SenderNumber, PTransmission_medium =:= Transmission_medium],
					_Trickle_timer ! increment,
					case IsParentMember of   %% 1- check if the sender is a potential parent
						[] -> %io:format("New Parent! ~n"), %% the sender is not in my parents' set	
							Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
							MyRank = Rank + Link_cost,
							Parent = {{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank},
							%NewParentSet = sort([Parent]++_Parent_Set),
							NewParentSet = sort(_Parent_Set,Parent),
							{_,_,_,_PNumberOfHops_old,_,_,_,_PMy_Rank_old} = lists:nth(1, _Parent_Set),		  
							{{_Parent_Name,_Trans_Parent},_,_,_PNumberOfHops,_,_,_,_PMy_Rank} = lists:nth(1, NewParentSet),	
							%LeastPotential_Parent = lists:nth(length(_Parent_Set),_Parent_Set),
							%NewLeastPotentialParent = lists:nth(length(NewParentSet), NewParentSet),
							%NewPreferred_Parent = lists:nth(1,NewParentSet),
							%Preferred_Parent = lists:nth(1,_Parent_Set),
							CompareHead = headMatch(_Parent_Set,NewParentSet), %% checks match according to parent's rank!!!
							%CompareTail = tailMatch(_Parent_Set,NewParentSet), %% checks match according to parent's update time!!!
							case CompareHead of		%% 2- check if my preferred parent was changed
								true -> 
									NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,_PMy_Rank,VersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									spawn(fun() -> send_new_Rank(_StationNumber,_PMy_Rank) end),
%									case CompareTail of %% 3- check if the worst potential parent was changed
%										true -> ok;
%										{false, RefreshTime} ->
%											_Refresh_parent_set_timer ! {reset_timer,RefreshTime}
%									end,
									{next_state, dio_block, NewStateData};
								false -> %% ignore the message because it forces change of my rank - %%%%%%%%%%%%%%%%%%%% think again
									NewStateData = [_StationNumber,_Parent_Set,_DaoMessageList,_PMy_Rank_old,VersionNumber,_PNumberOfHops_old+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
									spawn(fun() -> send_new_Rank(_StationNumber,_PMy_Rank) end),
									%spawn(fun() -> send_new_Pref_Parent(,_StationNumber,))
									{next_state, dio_block, NewStateData}
							end;
						[{_,_,RR}] -> 
							%io:format("existing parent! ~n"), %% the sender is in my parents' set
							Pred = fun({{PSenderID,PTransmissionMedium},_PRank1,_PVR,_Pnoh,_PSINR,_PBITRATE,_PTN,_PMR}) -> {PSenderID,PTransmissionMedium} /= {SenderNumber,Transmission_medium} end, 
							ReducedParentSet = lists:filter(Pred, _Parent_Set),
							%io:format("the reduce set is ~p~n", [ReduceSet]),
							%ReducedParentSet = [{{PSenderNumber,PTransmission_medium},PRank1,PVersionNumber1,PNumberOfHops1,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},PRank1,PVersionNumber1,PNumberOfHops1,PSINR,PBitrate,PTimeNow,PMyRank} <- _Parent_Set, PSenderNumber /= SenderNumber, PTransmission_medium /= Transmission_medium],
							%io:format("the former parent set is: ~p~n, the reduced parent set is: ~p~n", [_Parent_Set,ReducedParentSet]),
							Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
							MyRank_tmp = Rank + Link_cost,
							MyRank = round(?Alpha*MyRank_tmp + (1-?Alpha)*RR),
							Parent = {{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,MyRank},
							%NewParentSet = sort([Parent]++ReducedParentSet),
							NewParentSet = sort(ReducedParentSet,Parent),
							%io:format("the new parent set is: ~p~n",[NewParentSet]),
							CompareHead = headMatch(_Parent_Set,NewParentSet), %% checks match according to parent's rank!!!
							CompareTail = tailMatch(_Parent_Set,NewParentSet), %% checks match according to parent's update time!!!
							{_,_,_PVersionNumber,_PNumberOfHops,_,_,_,PMyRank} = lists:nth(1, NewParentSet),
							case CompareHead of 
								true -> 
									%io:format("The same preferred parent after change ~n"),
									if 
										PMyRank >= _Rank -> %%
											{next_state, dio_block, StateData};
										true ->	
											spawn(fun() -> send_new_Rank(_StationNumber,PMyRank) end),
											case CompareTail of
												true -> NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
												{false, RefreshTime} -> 
													_Refresh_parent_set_timer ! {reset_timer,RefreshTime},
													IsAlive = is_process_alive(_Refresh_parent_set_timer),
													case IsAlive of
														true -> NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
														false -> 
															New_Parents_refresh_timerPid = spawn(fun() -> parents_update_timer(MyPid,RefreshTime) end),
															erlang:link(New_Parents_refresh_timerPid),
															NewStateData = [_StationNumber,NewParentSet,_DaoMessageList,PMyRank,_PVersionNumber,_PNumberOfHops+1,_DAO_flag,_DAO_notify_timer,New_Parents_refresh_timerPid,_Trickle_timer]
													end
											end,
											{next_state, dio_block, NewStateData}
									end;
								false -> %% ignore the message because it forces change of my rank
									{next_state, dio_block, StateData}
							end; %% finish false and CompareHead
					      _ ->  io:format("Unexpected case - incoming DIO message for a station with lower rank that exists more than once in the parent set ~n"),
						   {next_state,active,StateData}
					end
			end
	end;	

% incoming timeoout message - time to send a DAO message
dio_block({timeout,my_dao}, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case _DAO_flag of 
		1 -> ok;
		_ -> 
			{{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_SINR,_Bitrate,_TimeNow,_My_Rank} = lists:nth(1, _Parent_Set),
			Payload = {_StationNumber,SenderNumber,Transmission_medium},
			DaoMessage = {2, [_StationNumber,SenderNumber,Transmission_medium,Payload]},
			hyRPL:outmessage(DaoMessage) %% sending the message to bar's process!
	end,
    {next_state, dio_block , StateData};		

%% printing the state of the fsm
dio_block(print,StateData) -> 
 	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	io:format("My fields are: StationNumber: ~p~n, Parent_Set: ~p~n, Dao Message List: ~p~n, _Rank: ~p~n, VersionNumber: ~p~n, Number Of Hops: ~p~n, DAO_flag: ~p~n, DAO_notify_timer: ~p~n, _Refresh_parent_set_timer: ~p~n, _Trickle_timer: ~p~n",[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]),
	{next_state, dio_block , StateData};

dio_block(stop, StateData) ->
	 [_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	 %_DAO_notify_timer ! stop, 
	 %_Refresh_parent_set_timer ! stop,
	 %_Trickle_timer ! stop,
	 %parseMessageIn ! stop,
	 io:format("Station ~p is terminated ~n",[_StationNumber]),
	{stop, normal, StateData};

% incoming timeout message - update my parent set
dio_block({refresh_parents_list, TimeNow}, StateData) ->
	%io:format("Refreshing parents' list!"),
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	%{MegaSec,Sec,MicroSec} = erlang:now(),
	%TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),							
	Pred = fun({{_SenderNumber,_Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank}) -> TimeNow - _PTimeNow < ?ConstantDelayParent end,
	NewParentSet = lists:filter(Pred, _Parent_Set),
	%io:format("the new parent set is ~p~n",[NewParentSet]),
	case NewParentSet of
		[] ->
			%io:format("ParentSet is empty! ~n"),
			_DAO_notify_timer ! stop,
			_Trickle_timer ! inconsistency,
			NewStateData = [StationNumber,NewParentSet,[],infinity,_VersionNumber,infinity,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
			spawn(fun() -> send_new_Pref_Parent(none,StationNumber,none) end),
			spawn(fun() -> send_new_Rank(StationNumber,infinity) end),
			{next_state, disconnect, NewStateData};
		_ -> 
			%io:format("ParentSet is not empty ~n"),
			{_,ParentUpdateTime} = findOldestParent(NewParentSet),
			MyPid = self(),
			%io:format("My pid is: ~p, The oldest parent time is: ~p~n",[MyPid,ParentUpdateTime]),
			New_ParentSet_timerPid = spawn(fun() -> parents_update_timer(MyPid,ParentUpdateTime) end),
			erlang:link(New_ParentSet_timerPid),
			%io:format("New timer started! The pid is ~p~n", [New_ParentSet_timerPid]),
			CompareHead = headMatch(_Parent_Set,NewParentSet), 
			case CompareHead of
				true -> 
					NewStateData = [StationNumber,NewParentSet,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,New_ParentSet_timerPid,_Trickle_timer],
					spawn(fun() -> send_new_Rank(StationNumber,_Rank) end),
					{next_state, dio_block, NewStateData};
				false ->  %% an unlikely case
					io:format("Error, unlikely case ~n"),
					_Trickle_timer ! inconsistency,
					{{_PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1, NewParentSet),	
					NewStateData = [StationNumber,NewParentSet,DaoMessageList,_PMyRank,_VersionNumber,_PNumberOfHops+1,1,_DAO_notify_timer,New_ParentSet_timerPid,_Trickle_timer],
					spawn(fun() -> send_new_Pref_Parent(_PSenderNumber,StationNumber,_PTransmission_medium) end),
					spawn(fun() -> send_new_Rank(StationNumber,_PMyRank) end),
					{next_state, dio_block , NewStateData}
			end
	end;

% incoming timeout - time to forward DAO message from the list of DAO messages
dio_block({forwardDAO_timeout,Desc,TimerPid}, StateData) ->
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case DaoMessageList of
		[] -> {next_state, dio_block, StateData};
		_ -> Pred = fun({DaoPayload,_TimerPid}) -> DaoPayload =:= Desc andalso _TimerPid =:= TimerPid end,
			 IsExist = lists:filter(Pred, DaoMessageList),
			 case IsExist of
				 [] -> {next_state, dio_block, StateData};
				 _ ->
					 NewDaoMessageList = DaoMessageList -- IsExist,
					 {{SenderNumber,Transmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1, _Parent_Set),
					 DaoMessage = {2,[StationNumber,SenderNumber,Transmission_medium,Desc]},
					 hyRPL:outmessage(DaoMessage),%% send the message to Bar's process!!!
					 NewStateData = [StationNumber,_Parent_Set,NewDaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer],
					 {next_state, dio_block, NewStateData}
			 end
	end;

% incoming DAO message
dio_block({2, [_SenderNumber,ReceiverNumber,Payload]}, StateData) ->
	[StationNumber,_Parent_Set,DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	{Descendant,Descendant_parent,_Transmission_medium} = Payload,
	MyPid = self(),
	_Trickle_timer ! increment,
	case ReceiverNumber of
		StationNumber -> 
			Pred = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station =:= Descendant end,
			Pred2 = fun({{Station1,StationParent1, _Transmission_med1},_TimerPid1}) -> Station1 =:= Descendant andalso (StationParent1 /= Descendant_parent orelse _Transmission_med1 /= _Transmission_medium) end,
			Dao_Former_Messages = lists:filter(Pred,DaoMessageList),
			DAO_listMessages = lists:filter(Pred2, Dao_Former_Messages),
			DaoMessage = Dao_Former_Messages -- DAO_listMessages,
			List = DaoMessageList -- DAO_listMessages,
			case DaoMessage of
				[] -> NewTimerPid = spawn(fun() -> timer_forward_dao(MyPid,Payload) end),
					  NewDao_MessageList = [{Payload,NewTimerPid}] ++ List,
					  NewStateData = [StationNumber,_Parent_Set,NewDao_MessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];		  
				_ ->
					 [{_Payload1,TimerPid1}] = DaoMessage,
					 TimerPid1 ! reset,
					 IsAlive = is_process_alive(TimerPid1),
					 case IsAlive of
						 true -> 
							 NewDao_MessageList = List,
							 NewStateData = [StationNumber,_Parent_Set,NewDao_MessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer];
						 false -> NewList = List -- DaoMessage,
								  NewTimerPid = spawn(fun() -> timer_forward_dao(MyPid,Payload) end),
								  NewDao_MSG_List = [{Payload,NewTimerPid}]++NewList,
								  NewStateData = [StationNumber,_Parent_Set,NewDao_MSG_List,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]
					 end
			end,
			{next_state, dio_block, NewStateData};		 		
		_ -> %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% add case of DAO message from existing decendant in the DAO message list which is not addressed to me
			Pred = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station =:= Descendant end,
			Deleted_DAO_messages = lists:filter(Pred, DaoMessageList),
			case Deleted_DAO_messages of
				[] -> NewStateData = StateData;
				_ -> 
					[TimerPid ! stop || {_Payload,TimerPid} <- Deleted_DAO_messages],
					Pred2 = fun({{Station,_Station_Parent,_Transmission_med},_TimerPid}) -> Station /= Descendant end,
					NewDao_MSG_List = lists:filter(Pred2, DaoMessageList),
					NewStateData = [StationNumber,_Parent_Set,NewDao_MSG_List,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]
			end,
			{next_state, dio_block, NewStateData}			
	end;

% incoming data message without a source-route path
dio_block({3,[_SenderNumber,ReceiverNumber,-1,Payload]}, StateData) -> 
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case ReceiverNumber of
		StationNumber -> 
			spawn(fun() -> hyRPL:send_data_stat(StationNumber,Payload) end),
			{{PSenderNumber,_PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,_PSINR,_PBitrate,_PTimeNow,_PMyRank} = lists:nth(1,_Parent_Set),
			DataMessage = {3, [StationNumber,PSenderNumber,-1,Payload], _PTransmission_medium},
			case _SenderNumber of
				'1' -> spawn(fun() -> hyRPL:up_data_counter_origen() end);
				_ -> ok
			end,
			hyRPL:outmessage(DataMessage); %% send the message to bar's process via the best transmission medium between the Station and its preferred parent: the last field in the Data message - should be discarded by bar's process

		%16#ffffffff ->  ok; % incoming broadcast data messages - in the application layer
		_ -> ok
	end,
	{next_state, dio_block, StateData};

% incoming data message with a source-route path
dio_block({3, [_SenderNumber, ReceiverNumber, Path, Payload]}, StateData) when is_list(Path) =:= true ->  %% path structure created by the root: 
																		  %% [Destination, MidStation_N, MidStation_N-1,...,MidStation_1,root]
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case ReceiverNumber of
		StationNumber ->
			spawn(fun() -> hyRPL:send_data_stat(StationNumber,Payload) end), 
			Path_length = length(Path),
			case Path_length of
				0 -> io:format("Received new data message ~n"),
					 ok; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 1 -> ok; 
						%% doing something with the payload - processing the message - in the application layer
				_ ->  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% try-catch
					{NewReceiver,StationNumber1,Transmission_medium} = lists:nth(Path_length, Path),
					if
						StationNumber1 =:= StationNumber -> 
							NewPath = lists:sublist(Path, Path_length-1),
							DataMessage = {3, [StationNumber,NewReceiver,NewPath,Payload], Transmission_medium},
							hyRPL:outmessage(DataMessage);	%% send the message to bar's process - the last field in the Data message is 'transmission_medium' and should be discarded by bar's process
						true -> io:format("Unexpected Path format")
					end	
			end;

		%16#ffffffff -> ok; % incoming broadcast data messages - in the application layer
		_ -> ok
	end,
	{next_state, dio_block, StateData};

% event-handler for all other unexpected incoming messages
dio_block(_Event, StateData) ->	
    io:format("Unexpected incoming message ~p. The message is ignored ~n",[_Event]),
    {next_state, dio_block , StateData}.

%% disconnect/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:StateName-2">gen_fsm:StateName/2</a>
-spec disconnect(Event :: timeout | term(), StateData :: term()) -> Result when
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
disconnect({1,[SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]}, StateData) ->
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,	
	if
		VersionNumber < _VersionNumber -> 
			_Trickle_timer ! increment,
			{next_state, disconnect, StateData};
		true ->
			case Rank of 
				infinity -> 
					_Trickle_timer ! increment,
					{next_state, disconnect, StateData};
				_ ->
					%io:format("incoming dio message ~n"),
					if 
						VersionNumber > _VersionNumber -> io:format("******************************************** Version-Number increased ***********************************~n ************************************************************************************************************************************************* ~n ************************************************************************************************************************************************* ~n");
						true -> ok
					end,
					_Trickle_timer ! inconsistency,
					Link_cost = objective_function(NumberOfHops+1,SINR,Bitrate,Transmission_medium),
					My_Rank = Rank + Link_cost,
					MyPid = self(),
					{MegaSec,Sec,MicroSec} = erlang:now(),
					TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),
				%	TrickleTimerPid = spawn(fun() -> trickle(MyPid) end),
					DAO_notifyTimerPid = spawn(fun() -> regular_timer(MyPid,?MY_DAO_DELAY,my_dao) end),
					Refresh_ParentSet_TimerPid = spawn(fun() -> parents_update_timer(MyPid,TimeNow) end),
					erlang:link(Refresh_ParentSet_TimerPid),
					spawn(fun() -> send_new_Pref_Parent(SenderNumber,StationNumber,Transmission_medium) end),
					spawn(fun() -> send_new_Rank(StationNumber,My_Rank) end),
								%% ******************************************************************** The Version Number should be updated!!! change _VersionNumber to VersionNumber in the next line *****************************************
					{next_state, dio_block , [StationNumber,[{{SenderNumber,Transmission_medium},Rank,VersionNumber,NumberOfHops,SINR,Bitrate,TimeNow,My_Rank}],[],My_Rank, VersionNumber,NumberOfHops+1,1,DAO_notifyTimerPid,Refresh_ParentSet_TimerPid,_Trickle_timer]}
			end
	end;

% incoming DAO message
disconnect({2, _}, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	_Trickle_timer ! increment,
	{next_state, disconnect, StateData};

disconnect({3, [_SenderNumber, ReceiverNumber, _Path, Payload]}, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	case ReceiverNumber of
		_StationNumber -> io:format("New data message received to ~p but the station is in state disconnect ~n",[_StationNumber]),
			spawn(fun() -> hyRPL:send_data_stat(_StationNumber,Payload) end);
		_ -> ok
	end,
	{next_state, disconnect, StateData};

% a trigger event from the trickle process to send DIO message
disconnect(timeout_trickle, StateData) ->
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	DioMessage = {1, [_StationNumber, list_to_atom(integer_to_list(16#ff)), _Rank, _VersionNumber,_NumberOfHops, 3]}, % send a broadcast DIO message via both mediums: PLC and RF
	hyRPL:outmessage(DioMessage),
	{next_state, disconnect , StateData};

disconnect(print,StateData) -> 
 	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	io:format("My fields are: StationNumber: ~p~n, Parent_Set: ~p~n, Dao Message List: ~p~n, _Rank: ~p~n, VersionNumber: ~p~n, Number Of Hops: ~p~n, DAO_flag: ~p~n, DAO_notify_timer: ~p~n, _Refresh_parent_set_timer: ~p~n, _Trickle_timer: ~p~n",[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer]),
	{next_state, disconnect, StateData};

disconnect(stop, StateData) ->
 	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,_Refresh_parent_set_timer,_Trickle_timer] = StateData,
	%_DAO_notify_timer ! stop,
	%_Refresh_parent_set_timer ! stop,
	%_Trickle_timer ! stop,
	%parseMessageIn ! stop,
	io:format("Station ~p is terminated ~n",[_StationNumber]),
	{stop, normal, StateData};	

% event-handler for all other unexpected incoming messages
disconnect(_Event, StateData) ->
    {next_state, disconnect, StateData}.

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


%% handle_syn%% @param Pid  - the process id of the main process
%% @param T - time duration of the timerc_event/4
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
	[StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,DAO_notify_timer,Refresh_parent_set_timer,Trickle_timer] = StateData,
	MyPid = self(),
	case Info of
		{_,FromPid,_} when is_pid(FromPid) =:= true ->
			case FromPid of
				Trickle_timer -> TrickleTimerPid = spawn(fun() -> trickle(MyPid,StationNumber) end),
						 erlang:link(TrickleTimerPid),
						 NewState = [StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,DAO_notify_timer,Refresh_parent_set_timer,TrickleTimerPid];
			
				DAO_notify_timer -> DAO_notifyTimerPid = spawn(fun() -> regular_timer(MyPid,?MY_DAO_DELAY,my_dao) end),
						erlang:link(DAO_notifyTimerPid),
						NewState = [StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,DAO_notifyTimerPid,Refresh_parent_set_timer,Trickle_timer];

				_ -> 
					ParserPid = whereis(parseMessageIn),
					case ParserPid of
						undefined -> % the parser process crushed
							Receiver_process_pid = spawn(fun() -> hyRPL:incoming_message(MyPid,StationNumber) end),
							register(parseMessageIn,Receiver_process_pid),
							erlang:link(Receiver_process_pid),
							NewState = StateData;
						_ -> io:format("normal Termination ~n"), 
						     NewState = StateData
					end
			end;
		_ -> NewState = StateData
	end,
    {next_state, StateName, NewState}.
%% @param Pid  - the process id of the main process
%% @param T - time duration of the timer

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
	[_StationNumber,_Parent_Set,_DaoMessageList,_Rank,_VersionNumber,_NumberOfHops,_DAO_flag,_DAO_notify_timer,Refresh_parent_set_timer,Trickle_timer] = StateData,
		Trickle_timer ! stop,
		Refresh_parent_set_timer ! stop,
		hyRPL:send_message_Fsm(terminate),
		_DAO_notify_timer ! stop,
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

% incoming_message(Fsm_pid) ->
% 	receive
% 		stop -> bye;
% 		Message -> io:format("incomming message! ~n"),
% 			Transmission_medium = lists:nth(1,Message),
% 			RSSI = lists:nth(2,Message),
% 			Payload = lists:nthtail(2,Message),
% 			Message_type = lists:nth(1,Payload),
% 			Payload_length = length(Payload),
% 			case Message_type of
% 				1 ->
% 					if
% 						Payload_length < 7 -> io:format("error, DIO message corrupted!~n");
% 						true ->
% 							SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
% 							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
% 							FirstByteRank = lists:nth(4,Payload),
% 							SecondByteRank = lists:nth(5,Payload),
% 							Rank1 = FirstByteRank*trunc(math:pow(2,8)) + SecondByteRank,
% 							MaxRank = trunc(math:pow(2,16)) -1,
% 							case Rank1 of
% 								MaxRank -> Rank = infinity;
% 								_ -> Rank = Rank1
% 							end,
% 							VersionNumber = lists:nth(6,Payload),
% 							NumberOfHops = lists:nth(7,Payload),
% 							case Transmission_medium of
% 								1 -> BitRate = ?BitRateRF;
% 								2 -> BitRate = ?BitRatePLC;
% 								_ -> io:format("unknown transmission medium. BitRate initialized to 1 by default~n"),
%  										 BitRate = 1
% 							end,
% 							io:format("Line before the sending the message to the fsm ~p~n",[Fsm_pid]),
% 							try
% 								gen_fsm:send_event(Fsm_pid, {Message_type,[SenderNumber,ReceiverNumber,Rank,VersionNumber,NumberOfHops,RSSI,BitRate,Transmission_medium]})  %% [SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]
% 							catch
% 								Error -> io:format("Error in sending the parsed message to the FSM process ~p~n",[Error])
% 							end
% 					end,
% 					incoming_message(Fsm_pid);
% 				2 ->
% 					if
% 						Payload_length < 6 -> io:format("error, DAO message corrupted!~n");
% 						true ->
% 							SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
% 							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
% 							Descendant = list_to_atom(integer_to_list(lists:nth(4,Payload))),
% 							Descendant_parent = list_to_atom(integer_to_list(lists:nth(5,Payload))),
% 							Trans_medium_Desc = lists:nth(6,Payload),
% 							Desc_Payload = {Descendant,Descendant_parent,Trans_medium_Desc},
% 							%io:format("The incoming Dao message is ~p~n",[{Message_type,[SenderNumber,ReceiverNumber,Desc_Payload]}])
% 							gen_fsm:send_event(Fsm_pid, {Message_type,[SenderNumber,ReceiverNumber,Desc_Payload]})  %{2, [_SenderNumber,ReceiverNumber,Payload]}
% 					end,
% 					incoming_message(Fsm_pid);
% 				3 ->
% 					if
% 						Payload_length < 5 -> io:format("error, Data message corrupted!~n");
% 						true ->
% 							SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
% 							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
% 							Length_Path = lists:nth(4,Payload),
% 							Length_tail = length(lists:nthtail(4,Payload)),
% 							if
% 								Length_tail < Length_Path ->
% 									Path = [],
% 									io:format("error, Data message corrupted! ~n"),
% 									incoming_message(Fsm_pid);
% 								true ->
% 									Path = lists:sublist(Payload,5,Length_Path)
% 							end,
% 							Parsed_path = parsePath(Path),
% 							Payload_length1 = lists:nth(5+Length_Path,Payload),
% 							Length_Remain = length(lists:nthtail(5+Length_Path,Payload)),
% 							if
% 								Length_Remain < Payload_length1 ->
% 									Payload1 = [],
% 									io:format("error, Data message corrupted! ~n"),
% 									incoming_message(Fsm_pid);
% 								true ->
% 									Payload1 = lists:sublist(Payload,5+Length_Path+1,Payload_length1)
% 							end,
% 							io:format("the Payload of the data message is ~p~n",[Payload1]),
% 							gen_fsm:send_event(Fsm_pid, {Message_type,[SenderNumber,ReceiverNumber,Parsed_path,Payload1]})
% 					end,
% 					incoming_message(Fsm_pid);
% 				4 ->
% 					if
% 						Payload_length =< 4 -> io:format("error, Data message corrupted!~n");
% 						true ->
% 							SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
% 							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
% 							MessageType = Message_type - 1,
% 							Payload_length1 = lists:nth(4,Payload),
% 							Length_tail = length(lists:nthtail(4,Payload)),
% 							if
% 								Length_tail < Payload_length1 ->
% 									Payload1 = [],
% 									io:format("error, Data message corrupted! ~n"),
% 									incoming_message(Fsm_pid);
% 								true ->
% 									Payload1 = lists:sublist(Payload,5,Payload_length1)
% 							end,
% 							gen_fsm:send_event(Fsm_pid, {MessageType, [SenderNumber,ReceiverNumber,-1,Payload1]})
% 					end,
% 					incoming_message(Fsm_pid);
% 				_ -> io:format("unknown message type, ignore the message!~n"),
% 						incoming_message(Fsm_pid)
% 			end
% 	end.

% hyRPL:outmessage({Message_type, [_SenderNumber, _ReceiverNumber, _Rank, _VersionNumber,_NumberOfHops, Transmission_medium]}) -> %% sending DIO message
% 	Rest = randbytes(58),
% 	Length_Rest = Rest*8,
% 	Zeros = <<0:Length_Rest>>,
% 	case _Rank of
% 		infinity -> Rank1 = trunc(math:pow(2,16))-1;
% 		_ -> Rank1 = _Rank
% 	end,
% 	Rank = <<Rank1:16>>,
% 	Payload = [Message_type,list_to_integer(atom_to_list(_SenderNumber)),list_to_integer(atom_to_list(_ReceiverNumber))]++bitstring_to_list(Rank)++[_VersionNumber,_NumberOfHops]++bitstring_to_list(Zeros);
% 	%io:format("The Payload of the outcoming message is ~p~n",[Payload]);
% 	%?ModemModule:send(Transmission_medium,Payload);

% hyRPL:outmessage({Message_type,[SenderNumber,ReceiverNumber,Transmission_medium,Payload]}) -> %% sending DAO message
% 	{Descendant,Descendant_parent,_Transmission_medium} = Payload,
% 	Rest = randbytes(58),
% 	Length_Rest = Rest*8,
% 	Zeros = <<0:Length_Rest>>,
% 	Payload_parse = [Message_type,list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber)),list_to_integer(atom_to_list(Descendant)),list_to_integer(atom_to_list(Descendant_parent)),_Transmission_medium]++bitstring_to_list(Zeros);
% 	%io:format("The Payload of the outcoming message is ~p~n",[Payload_parse]);
% 	%?ModemModule:send(Transmission_medium,Payload_parse);

% hyRPL:outmessage({Message_type, [SenderNumber,ReceiverNumber,Path,Payload], _PTransmission_medium}) -> %% Data messages
% 	case Path of
% 		-1 ->
% 			Type_of_message = Message_type + 1,  %% if there is no path, the type of message will be 4 in order to distinguish between data messages with path and without
% 			Payload_length = length(Payload),
% 			%Binary_Payload_Size = Payload_length*8,
% 			Rest = randbytes(61 - Payload_length),
% 			Length_Rest = Rest*8,
% 			Zero_Padding_Payload = <<0:Length_Rest>>,
% 			Payload_parse = [Type_of_message]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Payload_length]++Payload++bitstring_to_list(Zero_Padding_Payload);
% 			%io:format("The Payload of the outcoming message is ~p~n",[Payload_parse]);
% 			%?ModemModule:send(_PTransmission_medium,Payload_parse);
% 		_ ->
% 			Parsed_path = convert_path(Path),
% 			Length_Path = length(Parsed_path),
% 			%Path_size = Length_Path*8,
% 			Payload_length = length(Payload),
% 			%Payload_size = Payload_length*8,
% 			Rest = randbytes(61-Payload_length-Length_Path),
% 			Length_Rest = Rest*8,
% 			Zero_Padding_Payload = <<0:Length_Rest>>,
% 			%%Parsed_path = convert_path(Path),
% 			%%Zero_Padding_Payload = <<Payload:Payload_size>>,
% 			Payload_parse = [Message_type]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Length_Path]++Parsed_path++[Payload_length]++Payload++bitstring_to_list(Zero_Padding_Payload)
% 			%%io:format("The Payload of the outcoming message is ~p~n",[Payload_parse])
% 			%?ModemModule:send(_PTransmission_medium,Payload_parse)
% 	end.

% parsePath(Path) -> parsePath(Path,[]). %% should be implemented!
% parsePath(Path,List) when Path =:= [] -> lists:reverse(List);
% parsePath(Path,List) ->
% 	Path_Length = length(Path),
% 	if
% 		Path_Length >= 3 ->
% 			Descendant = list_to_atom(integer_to_list(lists:nth(1,Path))),
% 			Descendant_parent = list_to_atom(integer_to_list(lists:nth(2,Path))),
% 			Trans_medium_Desc = lists:nth(3,Path),
% 			Remaining_Path = lists:nthtail(3,Path),
% 			Next_Stop = {Descendant,Descendant_parent,Trans_medium_Desc},
% 			parsePath(Remaining_Path,[Next_Stop]++List);
% 		true ->
% 	    	io:format("error in path's parsing, incomplete path is returned ~n"),
% 				lists:reverse(List)
% 	end.

%% exported function that receives the message in binary format list!
%send_message_Fsm(Message) ->
%	%io:format("send message to parsing process ~n"),
%	parseMessageIn ! Message.

% %% the function below convert the path into a list of numbers
% convert_path(Path) -> convert_path(Path,[]).
% convert_path(Path,List) when Path =:= [] -> lists:flatten(lists:reverse(List));
% convert_path(Path,List) ->
% 	{NewReceiver,StationNumber,Transmission_medium} = lists:nth(1,Path),
% 	convert_path(lists:nthtail(1,Path),[[list_to_integer(atom_to_list(NewReceiver)),list_to_integer(atom_to_list(StationNumber)),Transmission_medium]]++List).

% %% randomize a number in the range between 1 to Number
% randbytes(Number) ->
% 	random:uniform(Number).


%send_message_Fsm(Message) ->
	%io:format("send message to parsing process ~n"),
%	parseMessageIn ! Message.

-spec objective_function(NumberOfHops :: number(),SINR :: number(),Bitrate :: number(),Transmission_medium :: number()) -> Result :: number().
%% The objective function calculates the cost of a link according to a given @params:
%% @param NumberOfHops - the number of stations the message should be forwarded through from the root to the destination
%% @param SINR - Signal Interference Noise Ratio
%% @param Bitrate - the speed the data is transmitted within the given transmission medium
%% @param Transmission_medium - RF or PLC

%% The Transmission medium is RF
objective_function(NumberOfHops,SINR,Bitrate,1) -> 
	?W1*NumberOfHops + ?W2*SINR + ?W3*Bitrate;

%% The Transmission medium is PLC
objective_function(NumberOfHops,SINR,Bitrate,2) -> 
	?W4*NumberOfHops + ?W5*SINR + ?W6*Bitrate.

%% this function checks whether the heads of both lists are equal in meaning of name and transmission medium
headMatch(List1,List2) when (List1 =:= [] orelse List2 =:= []) -> false;
headMatch(List1,List2) ->
	H1 = lists:nth(1, List1),
	H2 = lists:nth(1, List2),
	{SenderAndTransmission1,_,_,_,_,_,_,_} = H1,
	{SenderAndTransmission2,_,_,_,_,_,_,_} = H2,
	if
		SenderAndTransmission1 =:= SenderAndTransmission2 -> true;
		true -> false
	end.

% %% this function is a sort function according to the my relative rank via the element. 
% %% @param parents list
% sort([]) -> [];
% sort([H|[]]) -> [H];
% sort([H|T]) -> 
% 	{_,_,_,_,_,_,_,My_Rank_AccordingTo_Head} = H,
% 	sort([{{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} <- T, PMyRank < My_Rank_AccordingTo_Head])
% 	++ [H] ++ sort([{{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} <- T, PMyRank >= My_Rank_AccordingTo_Head]).

sort(List,Elem) when List =:= [] -> [Elem];
sort(List,Elem) ->
  {_,_,_,_,_,_,_,My_Rank_AccordingTo_Elem} = Elem,
  SmallerEquals_list = [{{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} <- List, PMyRank =< My_Rank_AccordingTo_Elem],
  Larger_list = [{{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} || {{PSenderNumber,PTransmission_medium},_PRank,_PVersionNumber,_PNumberOfHops,PSINR,PBitrate,PTimeNow,PMyRank} <- List, PMyRank > My_Rank_AccordingTo_Elem],
  SmallerEquals_list++[Elem]++Larger_list.
  
%% this function checks whether the oldest parents in both list are equal.
%% @param two parents list 
tailMatch(_Parent_Set,NewParentSet) ->
		{_OldestParent_OldSet,OldestParentTime_old_set} = findOldestParent(_Parent_Set),
		{_OldestParent_NewSet,OldestParentTime_new_set} = findOldestParent(NewParentSet),
		if
			OldestParentTime_old_set =:= OldestParentTime_new_set -> true;
			true -> {false, OldestParentTime_new_set}
		end.

%% a recursion function that findes the oldest parent in a given list
%% @param [H|T] - parents list
findOldestParent([]) -> [];
findOldestParent([H|T]) -> findOldestParent([H|T],{-1,math:pow(10,14)}).

findOldestParent([H|[]],{Parent,TimeCarry}) -> 
	{_,_,_,_,_,_,ParentTime,_} = H,
	if 
		ParentTime < TimeCarry -> {H,ParentTime};
		true -> {Parent,TimeCarry}
	end;

findOldestParent([H|T],{Parent,TimeCarry}) -> 
	{_,_,_,_,_,_,ParentTime,_} = H,
	if
		ParentTime < TimeCarry -> findOldestParent(T,{H,ParentTime});
		true -> findOldestParent(T,{Parent,TimeCarry})
	end.

%% A timer that notifies the main process when to send an incoming dao message from one of its descendant to its preferred parent. 
%% The timer has a pre-defined time duration - DAO_DELAY.
%% @param Fsm_Pid - the process id of the main process
%% @Desc - Descendant's name/id
timer_forward_dao(Fsm_Pid,Desc) -> 
	receive
		reset -> timer_forward_dao(Fsm_Pid,Desc); %% reset the timer
		stop -> ok
	after 
		?DAO_DELAY ->
					MyPid = self(),
					try
						gen_fsm:send_event(Fsm_Pid, {forwardDAO_timeout,Desc,MyPid})
					catch
						_:_ -> ok
					end
	end.

%% A timer that calculates its time duration and notifies the main process when to refresh its parents set.
%% @param Fsm_Pid - the process id of the main process
%% @param Time - the update time of the least updated parent
parents_update_timer(Fsm_Pid,Time) ->
	%io:format("new timer is running! the Fsm Pid is ~p~n", [Fsm_Pid]),
	{MegaSec,Sec,MicroSec} = erlang:now(),  %% 3 fields that represents the time now
	TimeNow = trunc((MegaSec*math:pow(10, 6)+Sec+MicroSec*math:pow(10, -6))*math:pow(10, 3)),  %% scaling - time now in resolution of millisec 
	Timeout = Time + ?ConstantDelayParent - TimeNow, %% calculating the time duration of the timer
	if
		Timeout < 0 -> Timeout_timer = 0;
		true -> Timeout_timer = Timeout
	end,
	receive
		{reset_timer,NewParentTime} -> parents_update_timer(Fsm_Pid,NewParentTime); %% reset message
		stop -> io:format("Parent_update timer is terminated ~n"), ok
	after
		Timeout_timer -> 	
				   {MegaSec1,Sec1,MicroSec1} = erlang:now(),
				   TimeNow1 = trunc((MegaSec1*math:pow(10, 6)+Sec1+MicroSec1*math:pow(10, -6))*math:pow(10, 3)),
				   try		
				   	gen_fsm:send_event(Fsm_Pid, {refresh_parents_list, TimeNow1})
				   catch
					_:_ -> ok
				   end
				   %gen_fsm:send_event(Fsm_Pid, {refresh_parents_list, TimeNow1}), %% just for security, sometimes the message is not received in the station's process
				   %io:format("I sent a refresh parent set timeout! ~n")
	end.
	
%% A regular timer that every T millisec sends notification to the main process
%% @param Pid  - the process id of the main process
%% @param T - time duration of the timer
regular_timer(Pid,T,Message) ->
	receive
		{update_timeout,Time} -> regular_timer(Pid,Time,Message);
		stop -> ok
	after
		T -> 
			try			
				gen_fsm:send_event(Pid, {timeout,Message})
			catch
				_:_ -> ok
			end,
			regular_timer(Pid,T,Message)
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
	trickle(Fsm_Pid,Fsm_Name,1,?Imin,Timer_pid).

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
							try
								gen_fsm:send_event(Fsm_Pid, timeout_trickle)
							catch
								_:_ -> ok
							end, 
							if %% updating the value of I
								I < ?Imax -> Inew = 2*I,
									     spawn(fun() -> sendNew_I_Server(Inew,Fsm_Name) end); %% sending the new I to the Local server
								true -> Inew = I
							end, 
							Tnew = round(0.5*(RandomT+1)*Inew), %% calculating the new time duration of the timer
							PidNew = spawn(fun() -> trickle_timer(TricklesPid,Tnew) end), %% initializing the timer with time duration Tnew
							trickle(Fsm_Pid,Fsm_Name,0,Inew,PidNew);
					true ->  %% sending dio message's notification SHOULD NOT be sent
						if 
							I > ?Imin -> Inew = trunc(0.5*I), %% updating the value of I
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
			trickle(Fsm_Pid,Fsm_Name,C+1,I,Timer_pid);
		
		stop -> Timer_pid ! stop, 
			io:format("The trickle timer is terminated ~n"),
			ok
	end.

sendNew_I_Server(Inew,Fsm_Name) -> %ok.
	%io:format("The new I of the trickle in station ~p is ~p ~n",[Fsm_Name,Inew]),
	?LocalServer:send({newI,Inew,Fsm_Name}).

send_incons_event_msg({inconsistency,Fsm_Name}) -> %ok.
	%io:format("inconsistency event of StationNumber ~p ~n", [Fsm_Name]),
	?LocalServer:send({inconsistency,Fsm_Name}).

send_new_Pref_Parent(Parent_Name,Station_Name,Transmission_medium) -> %ok.
	%io:format("The new parent of Station ~p via medium ~p is ~p ~n",[Station_Name,Transmission_medium,Parent_Name]),
	?LocalServer:send({pref_parent,Station_Name,Parent_Name,Transmission_medium}).

send_new_Rank(StationNumber,Rank) ->
	?LocalServer:send({new_Rank,StationNumber,Rank}).
