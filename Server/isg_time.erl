-module(isg_time).
-author("BarLesh").

-export([now_now/0, timeStamp_to_Time/1]).



now_now() -> 
		{MSec,Sec,_USec} = os:timestamp(), T1 = 1000000*MSec+Sec+10800, 
		Base = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}), 
		Base + T1.


fix_time(T) when T<10 -> "0" ++ integer_to_list(T);
fix_time(T)-> integer_to_list(T).


timeStamp_to_Time(never)->never;
timeStamp_to_Time(Time) ->
	
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Time),
	Sec_Str = fix_time(Sec),
	Min_Str = fix_time(Min),
	Hour_Str = fix_time(Hour),	
	A =   Hour_Str ++ ":" ++  Min_Str ++ ":" ++  Sec_Str,
	A2 = integer_to_list(Day) ++ "-" ++  integer_to_list(Month) ++ "-" ++  integer_to_list(Year) ++ "||" ++ A, A2.
