-module(node).
-behaviour(gen_server).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%dev exports
-export([compile_resources/0]).


-export([start/0, start/1, stop/0]).
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
                  application_type,
                  protocol_monitor_ref,
                  protocol_properties,
                  logger_ref,
                  report_unit_monitor_ref,
                  report_unit_properties,
                  node_status_timer

                  }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
        GlobalProperties = read_props(),
        io:format("Globalprops: ~w ...~n", [GlobalProperties]),
        internal_start([{node_name, local_1}|GlobalProperties]).

start(ScriptPropertiesList) ->
        NodeName = lists:nth(1, ScriptPropertiesList),
        ApplicationMode = list_to_atom(lists:nth(2, ScriptPropertiesList)),
        DataLinkStartState = list_to_atom(lists:nth(3, ScriptPropertiesList)),
        GlobalProperties = read_props(),
        io:format("ApplicationMode: ~p , NodeName: ~p, DataLinkStartState: ~p.~n", [ApplicationMode, NodeName, DataLinkStartState]),
        NewGlobalProps2 = injectProperties(GlobalProperties, ?APPLICATION_PROPERTIES, {role, ApplicationMode}),
        NewGlobalProps = injectProperties(NewGlobalProps2, ?PROTOCOL_PROPERTIES, ?DATA_LINK_PROPERTIES, {default_state, DataLinkStartState}),
        io:format("NewGlobalProps: ~p ...~n", [NewGlobalProps]),
        internal_start([{node_name, NodeName}|NewGlobalProps]).


internal_start(Properties) when is_list(Properties)->
    compile_resources(),
    NodeProperties = proplists:get_value(?NODE_PROPERTIES, Properties),
    NodeName = proplists:get_value(node_name, Properties),
    Timeout = proplists:get_value(timeout, NodeProperties),
    io:format("[~p]: TimeOut = ~p~n", [?MODULE, Timeout]),
    {ok, NodePID} = gen_server:start_link({global, list_to_atom(NodeName)}, ?MODULE, Properties, [{timeout, Timeout * 3}]),
    %% Spawn Monitor
%    spawn(?MODULE, monitor_func, [NodePID, [NodeName, NodeRole]]),
    NodePID.

stop() ->
    gen_server:call(?MODULE, stop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Callback Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(GlobalProperties) ->
%	process_flag(trap_exit, true),
	group_leader(whereis(user),self()), %this process is a group leader for this node


	%initialize reporting-unit
	LoggerPID = ?LOGGER:start(),
	LoggerREF = erlang:monitor(process, LoggerPID),
	?LOGGER:debug("[~p]: Logger: ~p started with pid: ~p.~n", [?MODULE, ?LOGGER, LoggerPID]),


	NodeProperties = proplists:get_value(?NODE_PROPERTIES, GlobalProperties),
	NodeName = proplists:get_value(node_name, GlobalProperties),
	?LOGGER:debug("[~p]: Node Name: ~p~n", [?MODULE, NodeName]),
    NodeAddress = utils:get_node_number(NodeName),
	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	MAC = utils:get_mac(),
	IP = utils:get_ip(),
	?LOGGER:debug("[~p]: Node Name: ~p, Address: ~p,  IP: ~p, MAC: ~p~n", [?MODULE, NodeName, NodeAddress, IP, MAC]),



	%initialize reporting-unit
	ReportUnitProperties = [{node_name, NodeName} | proplists:get_value(?REPORT_UNIT_PROPERTIES, GlobalProperties)],
	ReportUnitPid = ?REPORT_UNIT:start(ReportUnitProperties),
	ReportUnitMonitorReference = erlang:monitor(process, ReportUnitPid),
	?LOGGER:debug("[~p]: Report Unit: ~p started with pid: ~p and monitored by node: ~p.~n", [?MODULE, ?REPORT_UNIT, ReportUnitPid ,NodeName]),
	?REPORT_UNIT:connect_to_data_server(),

	%initialize PROTOCOL
	ProtocolProperties = [{?SELF_ADDRESS, NodeAddress} | proplists:get_value(?PROTOCOL_PROPERTIES, GlobalProperties)],
	CurrentProtocol = proplists:get_value(protocol, NodeProperties),
	Protocol_Pid = ?PROTOCOL:start(CurrentProtocol, ProtocolProperties),
	Protocol_Monitor_Reference = erlang:monitor(process, Protocol_Pid),
	?LOGGER:debug("[~p]: Protocol: ~p started  started with pid: ~p and monitored by node: ~p.~n", [?MODULE, CurrentProtocol, Protocol_Pid, NodeName]),

    %initialize application
    ApplicationProperties = proplists:get_value(?APPLICATION_PROPERTIES, GlobalProperties),
    ApplicationType = proplists:get_value(role, ApplicationProperties),
    Meters_list = proplists:get_value(meters_list, ApplicationProperties),
	% Application_Pid = ?APPLICATION:start(ApplicationProperties),
	Application_Pid = ApplicationType:start_link({list_to_atom(NodeName), Protocol_Pid, Meters_list}),
    % ?PROTOCOL:hand_shake(Application_Pid),
	Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
	?LOGGER:debug("[~p]: Application started  started with pid: ~p and monitored by node: ~p.~n", [?MODULE, Application_Pid, NodeName]),

    ?LOGGER:info("[~p]: Node: ~p, is up.~n", [?MODULE, NodeName]),

    Timer = timer:send_interval(?NODE_STATUS_TIMER_INTERVAL, self(), send_node_status), % ~50 fps

    {ok, #context{
        node_properties = NodeProperties,
        node_name = NodeName,
        protocol_monitor_ref = Protocol_Monitor_Reference,
        protocol_properties = ProtocolProperties,
        application_monitor_ref = Application_Monitor_Reference,
        application_properties = {list_to_atom(NodeName), Protocol_Pid, Meters_list},
        application_type = ApplicationType,
        % application_properties = ApplicationProperties,
        report_unit_monitor_ref = ReportUnitMonitorReference,
        report_unit_properties = ReportUnitProperties,
        node_status_timer = Timer,
        logger_ref = LoggerREF
    }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CALL Request(~w) from ~p, Context: ~w~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_cast({update_configuration, OptionsList}, Context) ->
    ?LOGGER:debug("[~p]: CAST Request(update_configuration), Options: ~w, Context: ~w ~n", [?MODULE, OptionsList, Context]),
    ?PROTOCOL:update_configuration(OptionsList),
    {noreply, Context};


handle_cast({reset_node}, Context) ->
    ?LOGGER:debug("[~p]: CAST Request(reset_node)~n", [?MODULE]),
    ?PROTOCOL:reset(),
    {noreply, Context};


handle_cast({initiate_transaction, {Destination, Data}}, Context) ->
    ?LOGGER:debug("[~p]: CAST Request(initiate_transaction), Destination:~p, Data: ~p, Context: ~w ~n", [?MODULE, Destination, Data, Context]),
    ?PROTOCOL:send(utils:get_node_number(Destination), term_to_binary(Data)),
    {noreply, Context};


handle_cast(Request, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CAST Request(~w), Context: ~w ~n", [?MODULE, Request, Context]),
    {noreply, Context}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case Application crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{application_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("[~p]: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
    ApplicationType = Context#context.application_type,
    Application_Pid = ApplicationType:start_link(Context#context.application_properties),
    % Application_Pid = ?APPLICATION:start(Context#context.application_properties),
    % ?PROTOCOL:hand_shake(Application_Pid),
    Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
    NewContext = Context#context{application_monitor_ref = Application_Monitor_Reference},
    {noreply, NewContext};

%case Report Unit crashed, restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{report_unit_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("[~p]: Report Unit crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
    ReportUnitPid = ?REPORT_UNIT:start(Context#context.report_unit_properties),
    ReportUnitMonitorReference = erlang:monitor(process, ReportUnitPid),
    NewContext = Context#context{report_unit_monitor_ref = ReportUnitMonitorReference},
    {noreply, NewContext};

%case Protocol crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{protocol_monitor_ref = Monitor_Ref} = Context)  ->
    CurrentProtocol = proplists:get_value(protocol, Context#context.node_properties),
    ?LOGGER:debug("[~p]: Protocol ~p crashed on node ~p, reason: ~p, restarting protocol.~n",[?MODULE, CurrentProtocol, Context#context.node_name, Reason]),
    ProtocolPid = ?PROTOCOL:start(CurrentProtocol, Context#context.protocol_properties),
    Protocol_Monitor_Reference = erlang:monitor(process, ProtocolPid),
    NewContext = Context#context{application_monitor_ref = Protocol_Monitor_Reference},
    {noreply, NewContext};

%case Protocol crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{logger_ref = Monitor_Ref} = Context)  ->
    LoggerPID = ?LOGGER:start(),
    ?LOGGER:debug("[~p]: Logger ~p crashed on node ~p, reason: ~p, restarting Logger.~n",[?MODULE, ?LOGGER, Context#context.node_name, Reason]),
    LoggerREF = erlang:monitor(process, LoggerPID),
    NewContext = Context#context{logger_ref = LoggerREF},
    {noreply, NewContext};

handle_info(send_node_status, Context)  ->
    ?LOGGER:preciseDebug("[~p]: Handle INFO Request(send_node_status)~n", [?MODULE]),
    Status = ?PROTOCOL:get_status(),
    ?REPORT_UNIT:report(?NODE_STATUS_REPORT, Status),
	{noreply, Context};

handle_info(Request, Context)  ->
    ?LOGGER:info("[~p]: STUB Handle INFO Request(~w), Context: ~w~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    %TODO Proper termination of module with all consequences
    ?LOGGER:info("[~p]: STUB terminating, Reason ~p, State ~p.~n", [?MODULE, Reason, Context]),
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

injectProperties(GlobalProperties, FirstLevelPropertiesListName, SecondLevelPropsListName, {Key, Value})->
    DestinationPropertiesList = proplists:get_value(FirstLevelPropertiesListName, GlobalProperties),
    io:format("1 Level props: ~w~n", [DestinationPropertiesList]),

    NewFirstLevelPropertiesList = injectProperties(DestinationPropertiesList, SecondLevelPropsListName, {Key, Value}),

    io:format("Trying to delete: ~p , from: ~w...~n", [FirstLevelPropertiesListName, GlobalProperties]),
    NewGlobalList = proplists:delete(FirstLevelPropertiesListName,  GlobalProperties),

    [{FirstLevelPropertiesListName, NewFirstLevelPropertiesList} | NewGlobalList].

injectProperties(GlobalProperties, DestinationPropertiesListName, {Key, Value})->
    DestinationPropertiesList = proplists:get_value(DestinationPropertiesListName, GlobalProperties),
    NewGlobalList = proplists:delete(DestinationPropertiesListName,  GlobalProperties),

    io:format("Trying to delete: ~p , from: ~w...~n", [Key, DestinationPropertiesList]),
    NewList = proplists:delete(Key,  DestinationPropertiesList),
    [{DestinationPropertiesListName, [{Key,Value} | NewList] } | NewGlobalList].


% no compilation errors handling, should not be errors
compile_resources() ->
	io:format("[~p]: Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	Results = [compile:file(File) || File <- ?NODE_RESOURCES],
    ?LOGGER:debug("Compilation result : ~p.~n", [Results]),
    case lists:member(error, Results) of
        true -> erlang:error("Compilation Error");
        false -> ?LOGGER:info("Resources compilation finished.~n")
    end.
