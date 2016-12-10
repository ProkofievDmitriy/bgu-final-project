-module(node).
-behaviour(gen_server).

-include("./include/properties.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(NODE, node_id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {name, mac, ip}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Params) ->
    log:info("Node starting ...~n"),
    compile_resources(),
    gen_server:start_link({local, ?NODE }, ?MODULE, Params, []).

stop() ->
    gen_server:call(?NODE, stop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Params) ->
	process_flag(trap_exit, true),
	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	{HostName, ID} = get_id(),
	Name = HostName,
	MAC = get_mac(),
	IP = get_ip(),
	?LOGGER:debug("Node ID: ~p,Name: ~p, IP: ~p, MAC: ~p~n", [ID, Name, IP, MAC]),

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

%	Pid_app = app:start(ID),
%	App_Ref = erlang:monitor(process, Pid_app),
%	io:format("App initiated and monitored!~n"),

    ?LOGGER:info("Node is up.~n"),

    {ok, #context{
        name = Name,
        mac = MAC,
        ip = IP
    }}.



%call for termination of node gen server
handle_call(Request, From, State) ->
    ?LOGGER:debug("~p STUB Handle CALL Request(~p) from ~p, State : ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOGGER:debug("~p STUB Handle CAST Request(~p), State : ~p ~n", [?MODULE, Request, State]),
    {noreply, State}.




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

get_id() ->
	Host = os:cmd("hostname"),
	?LOGGER:debug("at get_id: hostname is:~p~n", [Host]),
%	A = extract_id(Host),
	{remove_end_of_line(Host), todo_node_id}.


compile_resources() ->
	?LOGGER:debug("~p Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	[compile:file(File) || File <- ?NODE_RESOURCES],
    ?LOGGER:debug("Resources compilation finished.~n").