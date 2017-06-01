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
%%%		NumberServer of management message sent
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
-export([report/2, report/1, printStats/0]).
-export([export/0]).

%%%=======================================loadNGgui============================
%%% API
%%%===================================================================

%**************************************************************************************
%**********************   Management Server  		  	*******************************
%**************************************************************************************

export() ->
  gen_server:cast({global, ?STATS_SERVER}, {export_db, isg_time:now_now()}).



%**************************************************************************************
%**********************   	 From node  			    *******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	reports type:
%%	Message: {Type, Data}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Type: node_state
%%			{node_state,
%%				[{node_name,node_##},
%%				{routing_set,
%%					[{{destination,#},{next_address,#},{medium,1/2}},...]},
%%				{medium_mode,plc_only/rf_only/dual}]}
%%			This report will update the current state of the node in the server.
%%			routing_set should contain a list of all of the nodes neighbors.
%%				destination - final destination
%%				next_address - the closest neighbor
%%				medium - on which medium this transaction take place 1 - plc 2 - rf
%%			medium_mode should state the mode the node is in.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Type: node_is_down
%%			{node_is_down,DownNode}
%%				Tell the server this node is down and no longer running, and it should remove it.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%	Message to stats server:
%%
%%	Type: {management_message, sent_message}
%%		Report a management message was sent on node.
%%	Data:
%%			[{source,Source}, {destination, Destination}, {type, Type}]
%%
%%	Type: {management_message, received_message}
%%		Report a management message was received on node.
%%	Data:
%%			[{source,Source}, {destination, Destination}, {type, Type}]
%%
%%	Type: {data_message, sent_message}
%%		Report a data message was sent on node.
%%	Data:
%%			[{source,Source}, {destination, Destination}, {id, Id}]
%%
%%	Type: {data_message, received_message}
%%		Report a data message was received on node.
%%	Data:
%%			[{source,Source}, {destination, Destination}, {id, Id}]
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
report(Message) ->
	{Type, ReportData} = Message,
    report(Type, ReportData).


report(Type, Data) ->
	UTIME = isg_time:now_now(),
	io:format("REPORT to stats_server : Type: ~p, Utime: ~p, Data: ~p~n",[Type, UTIME, Data]),
	case Type of
		node_state ->
			Server = global:whereis_name(loadNGgui),
			wx_object:cast(Server, {Type, [{utime, UTIME} | Data]});
		_ ->
			gen_server:cast({global, ?STATS_SERVER}, {Type, [{utime, UTIME} | Data] })
    end.


%
%
% received_management_message (Source, Destination, Type) ->
% 	%UTIME = erlang:monotonic_tnode_is_upnode_is_upime(),
% 	UTIME = isg_time:now_now(),
% 	io:format("Sending to stats_server report about: Incoming management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
%
% 	gen_server:cast({global, ?STATS_SERVER}, {{management_message, sent_message}%},
% 											   [{utime, UTIME},
% 											   {source, Source},
% 											   {destination, Destination},
% 											   {message_type, Type}]
% 										   }).
%
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%  A node has Sent a management message  %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sent_management_message (Source, Destination, Type) ->
% 	UTIME = isg_time:now_now(),
% 	%UTIME = erlang:monotonic_time(),
% 	io:format("Sending to stats_server report about: Sent management msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
%
% 	gen_server:cast({global, ?STATS_SERVER}, {{management_message, sent_message}, [{utime,UTIME}, {source,Source}, {destination, Destination}, {type, Type}]}).
%
%
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%  A node has received a data message addressed for him  %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% received_data_message (Source, Destination, Id) ->
% 	UTIME = isg_time:now_now(),
% 	%UTIME = erlang:monotonic_time(),
% 	io:format("Sending to stats_server report about: Incoming msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
%
% 	gen_server:cast({global, ?STATS_SERVER}, {{data_message, received_message}, [{utime,UTIME}, {source,Source}, {destination, Destination}, {id, Id}]}).
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%  A node has Sent a data message  %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sent_data_message (Source, Destination, Id) ->
% 	UTIME = isg_time:now_now(),
% 	%UTIME = erlang:monotonic_time(),
% 	io:format("Sending to stats_server report about: Sent msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
%
% 	gen_server:cast({global, ?STATS_SERVER}, {{data_message, sent_message}, [{utime,UTIME}, {source,Source}, {destination, Destination}, {id, Id}]}).
%
%

%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printStats () ->
	io:format("Print Counters~n"),

	gen_server:cast({global, ?STATS_SERVER}, {printStats}).
