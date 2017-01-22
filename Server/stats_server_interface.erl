%%%-------------------------------------------------------------------
%%% @author Deddy Zagury
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%%	This module is the general stats server interface for the PLC+RF smart-meter project.
%%%
%%%	This module is used to send information to the stats server about the messages passing through the node.
%%%
%%%	The statical data that is gathered is:
%%%	
%%%		Number of data messages sent
%%%		Number of data messages received
%%%		Number of management message sent
%%%		Number of management message received
%%%
%%%	TODO:
%%%		Calculate average message time for arrival to destination.
%%%
%%% @end
%%% Created : 12. Jan 2017
%%%-------------------------------------------------------------------
-module(stats_server_interface).
-author("Deddy Zagury").


-define(STATS_SERVER, stats_server).

%% API
-export([received_data_message/2, sent_data_message/2, 
		 received_management_message/2, sent_management_message/2, 
		 printStats/0]).



%%%===================================================================
%%% API
%%%===================================================================

%**************************************************************************************
%**********************   Data Server  Updated  *******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a data message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
received_data_message (Source, Destination, Id) ->
	UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Incoming msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
	
	gen_server:cast({global, ?STATS_SERVER}, {{data_message, received_message}, [UTIME, Source, Destination, Id]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a data message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sent_data_message (Source, Destination, Id) ->
	UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Sent msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
	
	gen_server:cast({global, ?STATS_SERVER}, {{data_message, sent_message}, [UTIME, Source, Destination, Id]}).
	
%%--------------------------------------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a management message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
received_management_message (Source, Destination) ->
	UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Incoming management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),

	gen_server:cast({global, ?STATS_SERVER}, {{management_message, received_message}, [UTIME, Source, Destination]}).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a management message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sent_management_message (Source, Destination) ->
	UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Sent management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),

	gen_server:cast({global, ?STATS_SERVER}, {{management_message, sent_message}, [UTIME, Source, Destination]}).
	
%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printStats () ->
	io:format("Print Counters~n"),

	gen_server:cast({global, ?STATS_SERVER}, {printStats}).

