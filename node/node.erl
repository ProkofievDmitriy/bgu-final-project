-module(node).
-behaviour(gen_server).

-include("./include/properties.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dev exports
-export([compile_resources/0]).


-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {node_properties,
                  node_name,
                  ip,
                  mac,
                  application_monitor_ref,
                  application_properties,
                  protocol_monitor_ref,
                  protocol_properties,
                  report_unit_monitor_ref,
                  report_unit_properties

                  }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
        GlobalProperties = read_props(),
        io:format("GlobalProps: ~p ...~n", [GlobalProperties]),

        start(GlobalProperties).

start(Properties) when is_list(Properties)->
    compile_resources(),
%    NodeProperties = proplists:get_value(?NODE_PROPERTIES, Properties),
%    NodeName = proplists:get_value(node_name, NodeProperties),
    {ok,NodePID} = gen_server:start_link({global, ?MODULE}, ?MODULE, Properties, []),
    %% Spawn Monitor
%    spawn(?MODULE, monitor_func, [NodePID, [NodeName, NodeRole]]),
    NodePID.

stop() ->
    gen_server:call(?MODULE, stop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Callback Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(GlobalProperties) ->
	process_flag(trap_exit, true),
	group_leader(whereis(user),self()), %this process is a group leader for this node

	NodeProperties = proplists:get_value(?NODE_PROPERTIES, GlobalProperties),
	NodeName = proplists:get_value(node_name, NodeProperties),

	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	MAC = get_mac(),
	IP = get_ip(),
	?LOGGER:debug("Node Name: ~p, IP: ~p, MAC: ~p~n", [NodeName, IP, MAC]),

	%initialize reporting-unit
	ReportUnitProperties = proplists:get_value(?REPORT_UNIT_PROPERTIES, GlobalProperties),
	ReportUnitPid = ?REPORT_UNIT:start(ReportUnitProperties),
	ReportUnitMonitorReference = erlang:monitor(process, ReportUnitPid),
	?LOGGER:debug("Report Unit: ~p started and monitored by node: ~p.~n", [?REPORT_UNIT, NodeName]),

	%initialize PROTOCOL
	ProtocolProperties = proplists:get_value(?PROTOCOL_PROPERTIES, GlobalProperties),
	CurrentProtocol = proplists:get_value(protocol, NodeProperties),
	Protocol_Pid = ?PROTOCOL:start(CurrentProtocol, ProtocolProperties),
	Protocol_Monitor_Reference = erlang:monitor(process, Protocol_Pid),
	?LOGGER:debug("Protocol: ~p started and monitored by node: ~p.~n", [CurrentProtocol, NodeName]),

	%initiate modem_port module
%	Pid_modem = modem_port:start(),
%	Modem_Ref = erlang:monitor(process, Pid_modem),
%	io:format("modem_port initiated and monitored!~n"),

    %initialize application
    ApplicationProperties = proplists:get_value(?APPLICATION_PROPERTIES, GlobalProperties),
	Application_Pid = ?APPLICATION:start(ApplicationProperties),
	Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
	?LOGGER:debug("Application started and monitored by node: ~p.~n", [NodeName]),

    ?LOGGER:info("Node: ~p, is up.~n", [NodeName]),

    {ok, #context{
        node_properties = NodeProperties,
        node_name = NodeName,
        mac = MAC,
        ip = IP,
        protocol_monitor_ref = Protocol_Monitor_Reference,
        protocol_properties = ProtocolProperties,
        application_monitor_ref = Application_Monitor_Reference,
        application_properties = ApplicationProperties,
        report_unit_monitor_ref = ReportUnitMonitorReference,
        report_unit_properties = ReportUnitProperties
    }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Request, From, Context) ->
    ?LOGGER:debug("~p: STUB Handle CALL Request(~p) from ~p, Context : ~p~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(Request, Context) ->
    ?LOGGER:debug("~p: STUB Handle CAST Request(~p), Context : ~p ~n", [?MODULE, Request, Context]),
    {noreply, Context}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case Application crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{application_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("~p: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
    Application_Pid = ?APPLICATION:start(Context#context.application_properties),
    Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
    NewContext = Context#context{application_monitor_ref = Application_Monitor_Reference},
    {noreply, NewContext};

%case Report Unit crashed, restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{report_unit_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("~p: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
    ReportUnitPid = ?REPORT_UNIT:start(Context#context.report_unit_properties),
    ReportUnitMonitorReference = erlang:monitor(process, ReportUnitPid),
    NewContext = Context#context{report_unit_monitor_ref = ReportUnitMonitorReference},
    {noreply, NewContext};

%case Protocol crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{protocol_monitor_ref = Monitor_Ref} = Context)  ->
    CurrentProtocol = proplists:get_value(protocol, Context#context.node_properties),
    ?LOGGER:debug("~p: Protocol ~p crashed on node ~p, reason: ~p, restarting protocol.~n",[?MODULE, CurrentProtocol, Context#context.node_name, Reason]),
    ProtocolPid = ?PROTOCOL:start(CurrentProtocol, Context#context.protocol_properties),
    Protocol_Monitor_Reference = erlang:monitor(process, ProtocolPid),
    NewContext = Context#context{application_monitor_ref = Protocol_Monitor_Reference},
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

read_props() ->
    ApplicationProperties = ?APP_PROPS_LIST,
    NodeProperties = ?NODE_PROPS_LIST,
    ProtocolProperties = ?PROTOCOL_PROPS_LIST,
    ReportUnitProperties = ?REPORT_UNIT_PROPS_LIST,

    [{?APPLICATION_PROPERTIES, ApplicationProperties},
     {?NODE_PROPERTIES, NodeProperties},
     {?REPORT_UNIT_PROPERTIES, ReportUnitProperties},
     {?PROTOCOL_PROPERTIES, ProtocolProperties}
    ].


get_mac() ->
	M = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
    ?LOGGER:debug("get_mac: mac is:~p~n", [M]),
	remove_end_of_line(M).

get_ip() ->
    I = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"),
    ?LOGGER:debug("get_ip: ip is:~p~n", [I]),
	remove_end_of_line(I).

remove_end_of_line([])-> none;
remove_end_of_line([H|Tail])->
    case Tail == "\n" of
        true -> H;
        _ -> remove_end_of_line([H], Tail)
    end.

remove_end_of_line(Result, [H | []]) -> Result++[H];
remove_end_of_line(Result, [H | Tail]) when Tail =:= "\n" -> Result++[H];
remove_end_of_line(Result, [H | Tail]) -> remove_end_of_line(Result ++ [H], Tail).

% no compilation errors handling, should not be errors
compile_resources() ->
	io:format("~p: Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	Results = [compile:file(File) || File <- ?NODE_RESOURCES],
    ?LOGGER:debug("Compilation result : ~p.~n", [Results]),
    ?LOGGER:info("Resources compilation finished.~n").


