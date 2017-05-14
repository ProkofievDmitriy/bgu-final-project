%%%-------------------------------------------------------------------
%%% @author Deddy Zagury
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%%	This module is the general stats and management server interface for the PLC+RF smart-meter project.
%%%
%%%	This module is used to send information to the stats server about the messages passing through the node
%%% 	and to send information about the state of the node.
%%%
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

-define(MANAGMENT_SERVER, loadNGgui).

%% API
-export([received_data_message/3, sent_data_message/3, 
		 received_management_message/3, sent_management_message/3, 
		 printStats/0]).

-export([export/0]).

%%%===================================================================
%%% API
%%%===================================================================

export() -> 
  gen_server:cast({global, ?STATS_SERVER}, {export_db, isg_time:now_now()}).

%**************************************************************************************
%**********************   Management Server  		  	*******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node Update Status  								  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

node_is_up (NodeName, Data) ->
	io:format("Node ~p is up. data: ~w~n",[NodeName, Data]),
	Server = global:whereis_name(loadNGgui),
	wx_object:cast(Server, {node_is_up, {NodeName, Data}}).

node_is_up (NodeName, Plc,Rf,RoutingSet)->
	io:format("Node ~p State to: PLC- ~p, RF: ~p~n",[NodeName, Plc,Rf]),
	Server = global:whereis_name(loadNGgui),
	wx_object:cast(Server, {node_is_up,{NodeName,[{medium_mode,{Plc,Rf}},{routing_set,RoutingSet}]}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node is down  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_is_down (Id) ->
	io:format("Node is down with id ~p~n",[Id]),
		Server = global:whereis_name(loadNGgui),

	wx_object:cast(Server, {node_is_down,{Id}}	).




%**************************************************************************************
%**********************   	 Stats Server  			    *******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a management message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
received_management_message (Source, Destination, Type) ->
	%UTIME = erlang:monotonic_time(),
	UTIME = isg_time:now_now(),
	io:format("Sending to stats_server report about: Incoming management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),

	gen_server:cast({global, ?STATS_SERVER}, {{management_message, received_message}, [UTIME, Source, Destination, Type]}).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a management message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sent_management_message (Source, Destination, Type) ->
	UTIME = isg_time:now_now(),
	%UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Sent management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),

	gen_server:cast({global, ?STATS_SERVER}, {{management_message, sent_message}, [UTIME, Source, Destination, Type]}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has received a data message addressed for him  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
received_data_message (Source, Destination, Id) ->
	UTIME = isg_time:now_now(),
	%UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Incoming msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
	
	gen_server:cast({global, ?STATS_SERVER}, {{data_message, received_message}, [UTIME, Source, Destination, Id]}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  A node has Sent a data message  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sent_data_message (Source, Destination, Id) ->
	UTIME = isg_time:now_now(),
	%UTIME = erlang:monotonic_time(),
	io:format("Sending to stats_server report about: Sent msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
	
	gen_server:cast({global, ?STATS_SERVER}, {{data_message, sent_message}, [UTIME, Source, Destination, Id]}).
	


%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printStats () ->
	io:format("Print Counters~n"),

	gen_server:cast({global, ?STATS_SERVER}, {printStats}).

