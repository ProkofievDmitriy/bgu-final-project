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
%**********************   	 Stats Server  			    *******************************
%**************************************************************************************
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
% 	gen_server:cast({global, ?STATS_SERVER}, {{management_message, }%}%},
% 											   [{utime, UTIME},
% 											   {source, Source},anagement_message, received_mess
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
% 	gen_server:cast({global, ?STATS_SERVER}, {{management_message, sent_message}, [UTIME, Source, Destination, Type]}).
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
% 	gen_server:cast({global, ?STATS_SERVER}, {{data_message, received_message}, [UTIME, Source, Destination, Id]}).
%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%  A node has Sent a data message  %%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sent_data_message (Source, Destination, Id) ->
% 	UTIME = isg_time:now_now(),
% 	%UTIME = erlang:monotonic_time(),
% 	io:format("Sending to stats_server report about: Sent msg from ~p to ~p at ~p~n",[Source,Destination, UTIME]),
%
% 	gen_server:cast({global, ?STATS_SERVER}, {{data_message, sent_message}, [UTIME, Source, Destination, Id]}).
%
%

%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printStats () ->
	io:format("Print Counters~n"),

	gen_server:cast({global, ?STATS_SERVER}, {printStats}).
