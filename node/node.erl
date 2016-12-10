-module(node).
-behaviour(gen_server).

-include("./include/properties.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NODE, node_id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {name, role, mac, ip, protocol, report, server_port, application_monitor_ref}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(NodeName, NodeRole) ->
    log:info("Node starting ...~n"),
    compile_resources(),
    {ok,NodePID} = gen_server:start({global, NodeName}, ?MODULE, [NodeName, NodeRole], []),
    %% Spawn Monitor
    spawn(?MODULE, monitor_func, [NodePID, [NodeName, NodeRole]]),
    NodePID.

%start function to recover from monitor
start([NodeName | Tail]) ->
    log:info("~p: Node ~p recovering from crash throughout monitor with PARAMS: ~pn", [?MODULE, NodeName, Tail]),
    compile_resources(),
    {ok,NodePID} = gen_server:start({global, NodeName}, ?MODULE, [NodeName] ++ Tail, []),
    NodePID.

stop() ->
    gen_server:call(?NODE, stop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([NodeName, NodeRole]) ->
	process_flag(trap_exit, true),
	group_leader(whereis(user),self()), %this process is a group leader for this node
	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	MAC = get_mac(),
	IP = get_ip(),
	?LOGGER:debug("Node Name: ~p, IP: ~p, MAC: ~p~n", [NodeName, IP, MAC]),

	%init reporting-unit
%	{ok, Pid_server} = server_port:start_link(ID, MAC_Addr, IP_Addr, Node_Name),
%	Server_Ref = erlang:monitor(process, Pid_server),
%	io:format("local server port initiated and monitored!~n"),

	%initiate LOADng
%	Atom_ID = list_to_atom(integer_to_list(ID)),
%	Pid_HyLOADng = load_ng:start(Atom_ID),
%	HyLOADng_Ref = erlang:monitor(process, Pid_HyLOADng),
%	io:format("HyLOADng initiated and monitored! pid is:~p, Ref:~p~n",[Pid_HyLOADng, HyLOADng_Ref]),

	%initiate modem_port module
%	Pid_modem = modem_port:start(),
%	Modem_Ref = erlang:monitor(process, Pid_modem),
%	io:format("modem_port initiated and monitored!~n"),

	Application_Pid = ?APPLICATION:start(NodeRole, NodeName),
	Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
	?LOGGER:info("Application started and monitored by node.~n"),

    ?LOGGER:info("Node is up.~n"),

    {ok, #context{
        name = NodeName,
        role = NodeRole,
        mac = MAC,
        ip = IP,
        application_monitor_ref = Application_Monitor_Reference
    }}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Request, From, State) ->
    ?LOGGER:debug("~p: STUB Handle CALL Request(~p) from ~p, State : ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast(Request, State) ->
    ?LOGGER:debug("~p: STUB Handle CAST Request(~p), State : ~p ~n", [?MODULE, Request, State]),
    {noreply, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case Application crashed. restart it
handle_info( {'DOWN', Monitor_Ref , process, _Pid, Reason}, #context{application_monitor_ref = Monitor_Ref} = State)  ->
    ?LOGGER:info("~p: Application crashed on node ~p, reason: ~p, restarting application.~n",[?MODULE, State#context.name, Reason]),
    Application_Pid = ?APPLICATION:start(State#context.role, State#context.name),
    Application_Monitor_Reference = erlang:monitor(process, Application_Pid),
    NewState = State#context{application_monitor_ref = Application_Monitor_Reference},
    {noreply, NewState};

handle_info(Request, State)  ->
    ?LOGGER:debug("~p STUB Handle INFO Request(~p), State : ~p~n", [?MODULE, Request, State]),
	{noreply, State}.



terminate(Reason, State) ->
    ?LOGGER:debug("~p STUB terminating, reason ~p, state ~p~n", [?MODULE, Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mac() ->
	M = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
    ?LOGGER:debug("get_mac: mac is:~p~n", [M]),
	remove_end_of_line(M).

get_ip() ->
    I = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"),
    ?LOGGER:debug("get_ip: ip is:~p~n", [I]),
	remove_end_of_line(I).

remove_end_of_line([H|Tail])->
    case Tail == "\n" of
        true -> H;
        _ -> remove_end_of_line([H], Tail)
    end.

remove_end_of_line(Result, [H | []]) -> Result++[H];
remove_end_of_line(Result, [H | Tail]) when Tail =:= "\n" -> Result++[H];
remove_end_of_line(Result, [H | Tail]) -> remove_end_of_line(Result ++ [H], Tail).


compile_resources() ->
	?LOGGER:info("~p: Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	[compile:file(File) || File <- ?NODE_RESOURCES],
    ?LOGGER:info("Resources compilation finished.~n").



%% =================== Monitor Functions ====================
monitor_func(PID, Params) ->
	erlang:monitor(process, PID),
	start_monitoring(PID, Params).

start_monitoring(PID, Params) ->
	receive
		%% Server Is Down - Recovering...
		{'DOWN', _MonitorRef, _Type, _Object, Reason} ->
		    [NodeName| _Tail] = Params,
			?LOGGER:debug("~p: node ~p crashed,: ~p.~n", [?MODULE, NodeName, Reason]),
			?LOGGER:debug("~p: Restarting Node with parameters: ~p...~n", [?MODULE, Params]),
		    NewPID = ?MODULE:start(Params),
            erlang:monitor(process, NewPID),
            start_monitoring(NewPID, Params);
        _Else -> ok
	end.