-module(load_ng).
-behaviour(gen_server).

-include("./include/properties.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/1, stop/0, send/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {messages_queue,
                  data_link_monitor_ref,
                  data_link_properties,
                  load_ng_core_monitor_ref,
                  load_ng_core_properties
                }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Params) ->
    {ok,PID} = gen_server:start_link({local, ?MODULE }, ?MODULE, Params, []),
    PID.

stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


send({Destination, Data})-> gen_server:call({local, ?MODULE}, {data_message, {Destination, Data}}).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Params) ->
	?LOGGER:info("~p: Starting ... ~n", [?MODULE]),
%	process_flag(trap_exit, true),


	%initialize DATA_LINK
	DataLinkProperties = [],
	DataLinkPid = ?DATA_LINK:start(DataLinkProperties),
	DataLinkMonitorRef = erlang:monitor(process, DataLinkPid),
	?LOGGER:info("Data Link: ~p started and monitored by : ~p.~n", [?DATA_LINK, ?MODULE]),

	%initialize LOAD_NG_CORE
	LoadNgCoreProperties = [],
	LoadNgCorePid = ?LOAD_NG_CORE:start(LoadNgCoreProperties),
	LoadNgCoreMonitorRef = erlang:monitor(process, LoadNgCorePid),
	?LOGGER:info("LoadNG Core: ~p started and monitored by : ~p.~n", [?LOAD_NG_CORE, ?MODULE]),

    ?LOGGER:info("~p: is up as gen server~n", [?MODULE]),
    {ok, #context{
        messages_queue = [],
        data_link_monitor_ref = DataLinkMonitorRef,
        data_link_properties = DataLinkProperties,
        load_ng_core_monitor_ref = LoadNgCoreMonitorRef,
        load_ng_core_properties = LoadNgCoreProperties

    }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({data_message, {Destination, Data}}, Context=#context{messages_queue = MessagesQueue}) ->
    ?LOGGER:debug("~p  Handle CALL Request(data_message), Message: {~p, ~p}, Context : ~p~n", [?MODULE, Destination, Data, Context]),
    {reply, ok, Context}.





handle_call(Request, From, Context) ->
    ?LOGGER:debug("~p STUB Handle CALL Request(~p) from ~p, Context : ~p~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(Request, Context) ->
    ?LOGGER:debug("~p STUB Handle CAST Request(~p), Context : ~p ~n", [?MODULE, Request, Context]),
    {noreply, Context}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case DATA LINK crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{data_link_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("~p: DATA LINK crashed, reason: ~p, restarting application.~n",[?MODULE, Reason]),
    DataLinkPid = ?DATA_LINK:start(Context#context.data_link_properties),
    DataLinkMonitorRef = erlang:monitor(process, DataLinkPid),
    NewContext = Context#context{data_link_monitor_ref = DataLinkMonitorRef},
    {noreply, NewContext};

%case LOADng Core crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{load_ng_core_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("~p: LOADng crashed, reason: ~p, restarting application.~n",[?MODULE, Reason]),
    LoadNgCorePid = ?LOAD_NG_CORE:start(Context#context.load_ng_core_properties),
    LoadNgCoreMonitorRef = erlang:monitor(process, LoadNgCorePid),
    NewContext = Context#context{data_link_monitor_ref = LoadNgCoreMonitorRef},
    {noreply, NewContext};

handle_info(Request, Context)  ->
    ?LOGGER:debug("~p STUB Handle INFO Request(~p), Context : ~p~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    ?LOGGER:debug("~p STUB terminating, reason ~p, state ~p~n", [?MODULE, Reason, Context]),
    ok.

code_change(_OldVsn, Context, _Extra) -> {ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
