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
        io:format("Globalprops: ~w ...~n", [GlobalProperties]),
        internal_start([{node_name, local_1}|GlobalProperties]).

start([NodeName|_Tail]) ->
        GlobalProperties = read_props(),
        io:format("Globalprops: ~w ...~n", [GlobalProperties]),
        internal_start([{node_name, NodeName}|GlobalProperties]).

internal_start(Properties) when is_list(Properties)->
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
	NodeName = proplists:get_value(node_name, GlobalProperties),
	?LOGGER:debug("[~p]: Node Name: ~p~n", [?MODULE, NodeName]),
    NodeAddress = get_node_number(NodeName),
	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	MAC = get_mac(),
	IP = get_ip(),
	?LOGGER:debug("[~p]: Node Name: ~p, Address: ~p,  IP: ~p, MAC: ~p~n", [?MODULE, NodeName, NodeAddress, IP, MAC]),

	%initialize reporting-unit
	ReportUnitProperties = proplists:get_value(?REPORT_UNIT_PROPERTIES, GlobalProperties),
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
	Application_Pid = ?APPLICATION:start(ApplicationProperties),
    ?PROTOCOL:hand_shake(Application_Pid),
	Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
	?LOGGER:debug("[~p]: Application started  started with pid: ~p and monitored by node: ~p.~n", [?MODULE, Application_Pid, NodeName]),

    ?LOGGER:info("[~p]: Node: ~p, is up.~n", [?MODULE, NodeName]),

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

%case Application crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{application_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("[~p]: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
    Application_Pid = ?APPLICATION:start(Context#context.application_properties),
    ?PROTOCOL:hand_shake(Application_Pid),
    Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
    NewContext = Context#context{application_monitor_ref = Application_Monitor_Reference},
    {noreply, NewContext};

%case Report Unit crashed, restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{report_unit_monitor_ref = Monitor_Ref} = Context)  ->
    ?LOGGER:debug("[~p]: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, Context#context.node_name, Reason]),
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

handle_info(Request, Context)  ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), Context: ~w~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    %TODO Proper termination of module with all consequences
    ?LOGGER:debug("[~p]: STUB terminating, Reason ~p, State ~w.~n", [?MODULE, Reason, Context]),
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

get_node_number(NodeName)->
%    NodeNameAsList = atom_to_list(NodeName),
    ?LOGGER:debug("[~p]: get_node_number NodeName: ~p~n", [?MODULE, NodeName]),
    {Address, []} = string:to_integer(cut_list_from_delimiter(NodeName, 95)), % 95 = "_".
    Address.

get_mac() ->
	M = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
    ?LOGGER:debug("[~p]: get_mac: mac is:~p~n", [?MODULE, M]),
	remove_end_of_line(M).

get_ip() ->
    I = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"),
    ?LOGGER:debug("[~p]: get_ip: ip is:~p~n", [?MODULE, I]),
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

cut_list_from_delimiter([], _Delimiter)-> [];
cut_list_from_delimiter([H|Tail], Delimiter)->
    ?LOGGER:preciseDebug("[~p]: cut_list_from_delimiter H: ~p, Tail: ~p, Delimiter: ~p ~n", [?MODULE, H, Tail, Delimiter]),
    case H == Delimiter of
        true ->
            ?LOGGER:preciseDebug("[~p]: cut_list_from_delimiter Result: ~p~n", [?MODULE, Tail]),
            Tail;
        _ -> cut_list_from_delimiter(Tail, Delimiter)
    end.

% no compilation errors handling, should not be errors
compile_resources() ->
	io:format("[~p]: Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	Results = [compile:file(File) || File <- ?NODE_RESOURCES],
    ?LOGGER:debug("Compilation result : ~p.~n", [Results]),
    case lists:member(error, Results) of
        true -> erlang:error("Compilation Error");
        false -> ?LOGGER:info("Resources compilation finished.~n")
    end.


