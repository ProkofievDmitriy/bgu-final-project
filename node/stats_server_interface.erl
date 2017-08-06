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
-export([report/2, report/1, getOffset/1]).
-export([clear_routing_tables/0,remove_station/1,update_medium/1,configuration_updated_from_gui/1,routing_tables_cleared_from_gui/0]).
-export([export/0]).

%%%=======================================loadNGgui============================
%%% APItry
%%%===================================================================

%**************************************************************************************
%**********************   Management Server  		  	*******************************
%**************************************************************************************

export() ->
  gen_server:cast({global, ?STATS_SERVER}, {export_db, isg_time:now_now()}).

%% ---------------------------------------------------------------------------------------------------------
%%                    Application -> Server functionality requests (for Deddy to fill)
%% ---------------------------------------------------------------------------------------------------------

clear_routing_tables() ->
    % clear routing tables on all nodes
    Server = global:whereis_name(loadNGgui),
    wx_object:cast(Server, {resetAllNodes}),
    {ok, cleared}.

remove_stations(ListOfNodes)->
    % remove all the nodes in ListOfNodes from the network
    Server = global:whereis_name(loadNGgui),
    wx_object:cast(Server, {remove_stations,ListOfNodes}),
    {ok,removed}.

update_medium(ListOfNodesAndMediums)->
    % update configurations for the given ListOfNodesAndMediums where each element is a tuple {NodeName, MediumType},
    % MediumType can be plc or rf or dual
    Server = global:whereis_name(loadNGgui),
    wx_object:cast(Server, {update_medium,ListOfNodesAndMediums}),

    {ok, updated}.

%% ---------------------------------------------------------------------------------------------------------
%%                    Server -> Application notifications (for Liya to fill)
%% ---------------------------------------------------------------------------------------------------------


% in case a configuration update was issued from gui interface, the server should notify the app by calling this function
% ListOfNodesAndMediums - each element is a tuple {NodeName, MediumType}, MediumType can be plc or rf or dual
configuration_updated_from_gui (_ListOfNodesAndMediums) ->
    {ok}.

% in case routing tables were cleared from gui interface, the server should notify the app by calling this function
routing_tables_cleared_from_gui() ->
    {ok}.




%**************************************************************************************
%**********************   	 Stats Server  			    *******************************
%**************************************************************************************
report(Message) ->
	{Type, ReportData} = Message,
    report(Type, ReportData).

internal_report(Type, Data)->
    % UTIME = isg_time:now_now(),
    ?LOGGER:preciseDebug("[~p]: REPORT to stats_server : Type: ~p, Data: ~p~n",[?MODULE, Type, Data]),
    case Type of
        node_state ->
            Server = global:whereis_name(loadNGgui),
            ?LOGGER:preciseDebug("[~p]: WxServer found : ~p~n",[?MODULE, Server]),
            Reply =(catch wx_object:cast(Server, {Type, Data})),
                Reply;
        _ ->
            Reply =(catch gen_server:cast({global, ?STATS_SERVER}, {Type, Data })),
                Reply
    end.

report(Type, Data)->
    case internal_report(Type, Data) of
        ok -> {ok , all_good};
        {ok, _} -> {ok , all_good};
        Error ->
            ?LOGGER:critical("[~p]: Error catched: ~w ~n", [?MODULE, Error]),
            {error, Error}
    end.

%%  ------------------------------------------------------------------
%%	-------------------   Time Sync API     ----------------------
%%  ------------------------------------------------------------------

getOffset (CurrentMillis) ->
	Reply =( catch gen_server:call({global, ?STATS_SERVER}, {get_time_offset, CurrentMillis})),
    case Reply of
        {ok, Offset} ->
            ?LOGGER:debug("[~p]: Time sync: old ~p, offset ~p~n",[unfiltered, CurrentMillis, Offset]),
            Offset;
    _ ->
        ?LOGGER:debug("[~p]: Time sync: ERROR OCCURED WHILE COMMUNICATING DATA SERVER, reply was: ~p, returning offset = 0~n",[unfiltered, Reply]),
        0
    end.
