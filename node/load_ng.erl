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
                  network_pid,
                  network_monitor_ref,
                  network_properties,
                  modem_port_monitor_ref,
                  modem_port_pid,
                  modem_port_restart_timer_interval,
                  modem_port_restart_timer_ref,
                  modem_port_properties,
                  transport_pid,
                  transport_monitor_ref,
                  transport_properties,
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


	%initialize NETWORK
	NetworkProperties = [{?SELF_ADDRESS, SelfAddress} | proplists:get_value(?NETWORK_PROPERTIES, Properties)],
	NetworkPid = ?NETWORK:start(NetworkProperties),
	NetworkMonitorRef = erlang:monitor(process, NetworkPid),
	?LOGGER:info("[~p]: LoadNG Core: ~p started  started with pid: ~p and monitored by : ~p.~n", [?MODULE, ?NETWORK, NetworkPid, ?MODULE]),

	%initialize TRANSPORT
	TransportProperties = proplists:get_value(?TRANSPORT_PROPERTIES, Properties),
	TransportPid = ?TRANSPORT:start(TransportProperties),
	TransportMonitorRef = erlang:monitor(process, TransportPid),
	?LOGGER:info("[~p]: TRANSPORT : ~p started  started with pid: ~p and monitored by : ~p.~n", [?MODULE, ?TRANSPORT, TransportPid, ?MODULE]),

    bind_levels(?NETWORK, NetworkPid, ?DATA_LINK, DataLinkPid),
    bind_levels(?TRANSPORT, TransportPid, ?NETWORK, NetworkPid),

    ?LOGGER:info("[~p]: is up as gen server~n", [?MODULE]),
    {ok, #context{
        messages_queue = [],
        data_link_monitor_ref = DataLinkMonitorRef,
        data_link_pid = DataLinkPid,
        data_link_properties = DataLinkProperties,
        network_pid = NetworkPid,
        network_monitor_ref = NetworkMonitorRef,
        network_properties = NetworkProperties,
        transport_pid = TransportPid,
        transport_monitor_ref = TransportMonitorRef,
        transport_properties = TransportProperties,
        modem_port_monitor_ref = ModemPortMonitorRef,
        modem_port_pid = ModemPortPid,
        modem_port_restart_timer_interval = 30000,
        modem_port_properties = ModemPortProperties

    }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({data_message, {Destination, Data}}, From, Context) ->
    ?LOGGER:info("[~p]: data_message, Message: {~w, ~w}, transport pid = ~p, Context: ~w~n", [?MODULE, Destination, Data, Context#context.transport_pid, Context]),
    Result = ?TRANSPORT:send(Context#context.transport_pid, {?DATA, Destination, Data}),
    ?LOGGER:preciseDebug("[~p]: data_message, Result : ~p~n", [?MODULE, Result]),
    {reply, Result, Context};

handle_call({data_request_message, {Destination}}, From, Context) ->
    ?LOGGER:info("[~p]: data_request_message, Destination: ~w, transport pid = ~p, Context: ~w~n", [?MODULE, Destination, Context#context.transport_pid, Context]),
    Result = ?TRANSPORT:send(Context#context.transport_pid, {?DREQ, Destination, []}),
    ?LOGGER:preciseDebug("[~p]: data_request_message, Result : ~p~n", [?MODULE, Result]),
    {reply, Result, Context};

handle_call({data_reply_message, {Destination, Data}}, From, Context) ->
    ?LOGGER:info("[~p]: data_reply_message, Message: {~w, ~w}, transport pid = ~p, Context: ~w~n", [?MODULE, Destination, Data, Context#context.transport_pid, Context]),
    Result = ?TRANSPORT:send(Context#context.transport_pid, {?DREP, Destination, Data}),
    ?LOGGER:preciseDebug("[~p]: data_reply_message, Result : ~p~n", [?MODULE, Result]),
    {reply, Result, Context};

handle_call({hand_shake, ApplicationPid}, From, Context) ->
    ?LOGGER:info("[~p]: Handle CALL Request(hand_shake), ApplicationPid: ~p, From : ~p, Context: ~w~n", [?MODULE, ApplicationPid, From, Context]),
    %TODO implement hand_shake with application
    NewContext = Context#context{application_pid = ApplicationPid},
    ?TRANSPORT:updateUpperLevelPid(NewContext#context.transport_pid, NewContext#context.application_pid),
    {reply, self(), NewContext};

handle_call(get_status, _From, Context) ->
    ?LOGGER:preciseDebug("[~p]: Handle CALL Request(get_status)~n", [?MODULE]),
    %TODO implement hand_shake with application
    NetworkStatus = ?NETWORK:get_status(Context#context.network_pid),
    DataLinkStatus = ?DATA_LINK:get_status(Context#context.data_link_pid),
    {reply, NetworkStatus ++ DataLinkStatus , Context};


handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CALL Request(~w) from ~p, Context: ~w~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 handle_cast({update_configuration, OptionsList}, Context) ->
     ?LOGGER:debug("[~p]: CAST Request(update_configuration), Options: ~w, Context: ~w ~n", [?MODULE, OptionsList, Context]),

     {noreply, Context};

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
    bind_levels(?NETWORK, Context#context.network_pid, ?DATA_LINK, DataLinkPid),
    DataLinkMonitorRef = erlang:monitor(process, DataLinkPid),
    NewContext = Context#context{data_link_monitor_ref = DataLinkMonitorRef, data_link_pid = DataLinkPid},
    ?LOGGER:info("[~p]: DATA LINK AND MODEM PORT RESTARTED with pid: ~p.~n",[?MODULE, DataLinkPid]),
    {noreply, NewContext};

%case LOADng Core crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{network_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("[~p]: LOADng CORE crashed, reason: ~p, restarting application.~n",[?MODULE, Reason]),
    NetworkPid = ?NETWORK:start(Context#context.network_properties),
    bind_levels(?NETWORK, NetworkPid, ?DATA_LINK, Context#context.data_link_pid),
    bind_levels(?TRANSPORT, Context#context.transport_pid, ?NETWORK, NetworkPid),
    NetworkMonitorRef = erlang:monitor(process, NetworkPid),
    NewContext = Context#context{data_link_monitor_ref = NetworkMonitorRef, network_pid = NetworkPid},
    ?LOGGER:info("[~p]: LOADng CORE RESTARTED with pid: ~p.~n",[?MODULE, NetworkPid]),
    {noreply, NewContext};

%case Transport Core crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{transport_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:info("[~p]: TRANSPORT crashed, reason: ~p, restarting ...~n",[?MODULE, Reason]),
    TransportPid = ?TRANSPORT:start(Context#context.transport_properties),
    bind_levels(?TRANSPORT, TransportPid, ?NETWORK, Context#context.network_pid),
    TransportMonitorRef = erlang:monitor(process, TransportPid),
    NewContext = Context#context{transport_monitor_ref = TransportMonitorRef, transport_pid = TransportPid},
    ?LOGGER:info("[~p]: TRANSPORT restarted with pid: ~p.~n",[?MODULE, TransportPid]),
    {noreply, NewContext};

%case Transport Core crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, Pid, Reason}, #context{modem_port_monitor_ref = Monitor_Ref} = Context) ->
    ?LOGGER:info("[~p]: MODEM_PORT crashed, starting TIMER (~p)  to next restart ...~n",[?MODULE, Context#context.modem_port_restart_timer_interval]),
    TimerRef = erlang:start_timer(Context#context.modem_port_restart_timer_interval, self(), {'DOWN', Monitor_Ref , process, Pid, Reason} ),
    {noreply, Context#context{modem_port_restart_timer_ref = TimerRef}};

handle_info( {timeout, TimerRef , {'DOWN', Monitor_Ref , process, _Pid, Reason}}, #context{modem_port_monitor_ref = Monitor_Ref, modem_port_restart_timer_ref = TimerRef} = Context) ->
    ?LOGGER:info("[~p]: MODEM_PORT crashed, reason: ~p, restarting ...~n",[?MODULE, Reason]),
    ?MODEM_PORT:stop(),
    ModemPortPid = ?MODEM_PORT:start(Context#context.data_link_pid),
    ModemPortMonitorRef = erlang:monitor(process, ModemPortPid),
    NewContext = Context#context{modem_port_monitor_ref = ModemPortMonitorRef, modem_port_pid = ModemPortPid},
    ?LOGGER:info("[~p]: MODEM_PORT restarted with pid: ~p.~n",[?MODULE, ModemPortPid]),
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

bind_levels(UpperLevelModule, UpperLevelPid, BottomLevelModule, BottomLevelPid)->
    UpperLevelModule:updateBottomLevelPid(UpperLevelPid, BottomLevelPid),
    BottomLevelModule:updateUpperLevelPid(BottomLevelPid, UpperLevelPid).
