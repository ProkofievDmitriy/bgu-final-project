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
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-define(STATS_SERVER, stats_server).

-define(MANAGMENT_SERVER, loadNGgui).

%% API
-export([report/3, report/2, report/1, printStats/0]).
-export([export/0]).

%%%=======================================loadNGgui============================
%%% APItry
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

internal_report(Type, Data)->
    UTIME = isg_time:now_now(),
    ?LOGGER:preciseDebug("[~p]: REPORT to stats_server : Type: ~p, Utime: ~p, Data: ~p~n",[?MODULE, Type, UTIME, Data]),
    case Type of
        node_state ->
            Server = global:whereis_name(loadNGgui),
            ?LOGGER:preciseDebug("[~p]: WxServer found : ~p~n",[?MODULE, Server]),
            Reply =(catch wx_object:cast(Server, {Type, [{utime, UTIME} | Data]})),
                Reply;
        _ ->
            Reply =(catch gen_server:cast({global, ?STATS_SERVER}, {Type, [{utime, UTIME} | Data] })),
                Reply
    end.
report(Type, Data) ->
    report(Type, Data, undefined).

report(Type, Data, GrafanaServerIP)->
    case GrafanaServerIP of
        undefined -> ok;
        _ -> grafana_report(Type,GrafanaServerIP)
    end,

    case internal_report(Type, Data) of
        ok -> {ok , all_good};
        {ok, _} -> {ok , all_good};
        Error ->
            ?LOGGER:critical("[~p]: Error catched: ~w ~n", [?MODULE, Error]),
            {error, Error}
    end.

grafana_report(Type, GrafanaServerIP)->
    ?LOGGER:preciseDebug("[~p]: grafana_report Type = : ~w ~n",[?MODULE, Type]),
    case Type of
        {management_message,send_message} ->
            exec_curl(GrafanaServerIP, "loadng", "mgmt_msg", "1");
        {data_message,send_message} ->
            exec_curl(GrafanaServerIP, "loadng", "data_msgs", "1");
        {data_message,relay_message} ->
            exec_curl(GrafanaServerIP, "loadng", "data_msgs", "1");

        _ -> ok
    end.

exec_curl(GrafanaServerIP, DataBase, Table, Value)->
    Result = os:cmd("curl -i -XPOST 'http://" ++ GrafanaServerIP ++ ":8086/write?db=" ++ DataBase ++ "' --data-binary '" ++ Table ++" value=" ++ Value ++ "'"),
    ?LOGGER:preciseDebug("[~p]: exec_curl Result = : ~p ~n",[?MODULE, Result]).

%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printStats () ->
	io:format("Print Counters~n"),

	gen_server:cast({global, ?STATS_SERVER}, {printStats}).
