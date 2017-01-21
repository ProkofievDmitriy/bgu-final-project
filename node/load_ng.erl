-module(load_ng).
-behaviour(gen_server).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {messages_queue,
                  data_link_pid,
                  data_link_monitor_ref,
                  data_link_properties,
                  load_ng_core_pid,
                  load_ng_core_monitor_ref,
                  load_ng_core_properties,
                  modem_port_monitor_ref,
                  modem_port_properties,
                  application_pid
                }).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Properties) ->
	?LOGGER:info("[~p]: Starting LOADng with props: ~w~n", [?MODULE, Properties]),
%	process_flag(trap_exit, true),
    SelfAddress = proplists:get_value(?SELF_ADDRESS, Properties),
	%initialize DATA_LINK
	DataLinkProperties = [{?SELF_ADDRESS, SelfAddress} | proplists:get_value(?DATA_LINK_PROPERTIES, Properties)],
	DataLinkPid = ?DATA_LINK:start(DataLinkProperties),
	DataLinkMonitorRef = erlang:monitor(process, DataLinkPid),
	?LOGGER:info("[~p]: Data Link: ~p started  started with pid: ~p and monitored by : ~p.~n", [?MODULE, ?DATA_LINK, DataLinkPid, ?MODULE]),

	%initialize Mode Port module
	ModemPortProperties = [],
	ModemPortPid = ?MODEM_PORT:start(DataLinkPid),
	ModemPortMonitorRef = erlang:monitor(process, ModemPortPid),
	?LOGGER:debug("[~p]: Modem Port: ~p started  started with pid: ~p and monitored by ~p~n", [?MODULE, ?MODEM_PORT, ModemPortPid, ?MODULE]),


	%initialize LOAD_NG_CORE
	LoadNgCoreProperties = [{?SELF_ADDRESS, SelfAddress} | proplists:get_value(?LOAD_NG_CORE_PROPERTIES, Properties)],
	LoadNgCorePid = ?LOAD_NG_CORE:start(LoadNgCoreProperties),
	LoadNgCoreMonitorRef = erlang:monitor(process, LoadNgCorePid),
	?LOGGER:info("[~p]: LoadNG Core: ~p started  started with pid: ~p and monitored by : ~p.~n", [?MODULE, ?LOAD_NG_CORE, LoadNgCorePid, ?MODULE]),

    bind_levels(LoadNgCorePid, DataLinkPid),

    ?LOGGER:info("[~p]: is up as gen server~n", [?MODULE]),
    {ok, #context{
        messages_queue = [],
        data_link_monitor_ref = DataLinkMonitorRef,
        data_link_pid = DataLinkPid,
        data_link_properties = DataLinkProperties,
        load_ng_core_pid = LoadNgCorePid,
        load_ng_core_monitor_ref = LoadNgCoreMonitorRef,
        load_ng_core_properties = LoadNgCoreProperties,
        modem_port_monitor_ref = ModemPortMonitorRef,
        modem_port_properties = ModemPortProperties

    }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({data_message, {Destination, Data}}, From, Context=#context{messages_queue = MessagesQueue}) ->
    ?LOGGER:info("[~p]: Handle CALL Request(data_message), Message: {~p, ~p}, Context: ~w~n", [?MODULE, Destination, Data, Context]),
    Result = ?LOAD_NG_CORE:send(Context#context.load_ng_core_pid, {Destination, Data}),
    ?LOGGER:preciseDebug("[~p]: Handle CALL Request(data_message), Result : ~p~n", [?MODULE, Result]),
    {reply, Result, Context};


handle_call({hand_shake, ApplicationPid}, From, Context) ->
    ?LOGGER:info("[~p]: Handle CALL Request(hand_shake), ApplicationPid: ~p, From : ~p, Context: ~w~n", [?MODULE, ApplicationPid, From, Context]),
    %TODO implement hand_shake with application
    NewContext = Context#context{application_pid = ApplicationPid},
    {reply, self(), NewContext};


handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CALL Request(~w) from ~p, Context: ~w~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(Request, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CAST Request(~w), Context: ~w ~n", [?MODULE, Request, Context]),
    {noreply, Context}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case DATA LINK crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{data_link_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("[~p]: DATA LINK crashed, reason: ~p, restarting application.~n",[?MODULE, Reason]),
    DataLinkPid = ?DATA_LINK:start(Context#context.data_link_properties),
    ?MODEM_PORT:stop(),
    ?MODEM_PORT:start(DataLinkPid),
    bind_levels(Context#context.load_ng_core_pid, DataLinkPid),
    DataLinkMonitorRef = erlang:monitor(process, DataLinkPid),
    NewContext = Context#context{data_link_monitor_ref = DataLinkMonitorRef, data_link_pid = DataLinkPid},
    ?LOGGER:info("[~p]: DATA LINK AND MODEM PORT RESTARTED with pid: ~p.~n",[?MODULE, DataLinkPid]),
    {noreply, NewContext};

%case LOADng Core crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{load_ng_core_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("[~p]: LOADng CORE crashed, reason: ~p, restarting application.~n",[?MODULE, Reason]),
    LoadNgCorePid = ?LOAD_NG_CORE:start(Context#context.load_ng_core_properties),
    bind_levels(LoadNgCorePid, Context#context.data_link_pid),
    LoadNgCoreMonitorRef = erlang:monitor(process, LoadNgCorePid),
    NewContext = Context#context{data_link_monitor_ref = LoadNgCoreMonitorRef, load_ng_core_pid = LoadNgCorePid},
    ?LOGGER:info("[~p]: LOADng CORE RESTARTED with pid: ~p.~n",[?MODULE, LoadNgCorePid]),
    {noreply, NewContext};

handle_info(Request, Context)  ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), Context: ~w~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    %TODO Proper termination of module with all consequences
    ?LOGGER:debug("[~p]: STUB terminating, reason ~p, state ~p~n", [?MODULE, Reason, Context]),
    ok.

code_change(_OldVsn, Context, _Extra) -> {ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bind_levels(LoadNgCorePid, DataLinkPid)->
    ?LOAD_NG_CORE:updateBottomLevelPid(LoadNgCorePid, DataLinkPid),
    ?DATA_LINK:updateUpperLevelPid(DataLinkPid, LoadNgCorePid).