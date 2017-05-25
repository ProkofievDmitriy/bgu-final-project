%% This Module Is For Creating Some Math Function That Gets Degrees instead of radians
%% Added Random Functions


-module(mymath).
-define(EPS, 1.0e-10).

%% ====================================================================
%% API functions
%% ====================================================================
-export([cos/1, sin/1, atan/1, get_random/1, get_random_resource/1]).


cos(Degrees) -> 
	math:cos((Degrees * math:pi()) / 180).

sin(Degrees) ->
	math:sin((Degrees * math:pi()) / 180).

atan(Input) ->
	(math:atan(Input)*180)/math:pi().

get_random(Number)-> 
		rand:seed(erlang:timestamp()),
	{RandomNumber,_Seed} = rand:uniform_s(Number, rand:seed(erlang:timestamp())),
		RandomNumber.

get_random_resource(Number) ->
	%rand:seed(erlang:timestamp()),
	{A,B,C} = erlang:timestamp(),
	{RandomNumber, _Seed} = rand:uniform_s(Number, rand:seed(exs1024,{A,B,C})),
	RandomNumber.




%% ====================================================================
%% Internal functions
%% ====================================================================


%% cos(0) -> 1;
%% cos(90) -> 0;
%% cos(-90) -> 0;
%% cos(180) -> -1;
%% cos(-180) -> -1;
%% cos(Deg) when Deg >= 180 orelse Deg =< -180 -> cos(Deg rem 360);
%% cos(Deg) -> math:cos(Deg * math:pi() / 180).
