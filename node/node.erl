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

start() ->
    log:info("Node starting ...~n"),
    compile_resources(),
    gen_server:start_link({local,?NODE }, ?MODULE, [], []).

stop() ->
    gen_server:call(?NODE, stop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
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
    ?LOGGER:debug("~p Handle CALL Request(~p) from ~p, State : ~p~n", [?MODULE, Request, From, State]),
    {reply, ok, State}.

handle_cast(Request, State) ->
    ?LOGGER:debug("~p Handle CAST Request(~p), State : ~p ~n", [?MODULE, Request, State]),
    {noreply, State}.




handle_info(Request, State)  ->
    ?LOGGER:debug("~p Handle INFO Request(~p), State : ~p~n", [?MODULE, Request, State]),
	{noreply, State}.



terminate(Reason, State) ->
    ?LOGGER:debug("~p terminating, reason ~p, state ~p~n", [?MODULE, Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_mac() ->
	M = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
	delete_line_feed([], M).

get_ip() -> I = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"),
	delete_line_feed([], I).

extract_id2([H|_Rest], L) when H =:= 10 -> L;
extract_id2([H|Rest], L) -> L1 = L ++ [H], extract_id2( Rest, L1 ).

extract_id([H|Rest]) when H =:= 95 -> extract_id2(Rest, []);
extract_id([_H|Rest]) ->  extract_id(Rest).

delete_line_feed(M1, [M | Rest]) when Rest =:= "\n" -> M1++[M];
delete_line_feed(M1, [M | Rest]) -> M2 = M1 ++ [M], delete_line_feed(M2, Rest).

get_id() ->
	Host = os:cmd("hostname"),
	?LOGGER:debug("at get_id: hostname is:~p~n", [Host]),
%	A = extract_id(Host),
	{delete_line_feed([], Host), 1987}.


compile_resources() ->
	?LOGGER:debug("~p Compiling Resources: ~p~n", [?MODULE, ?NODE_RESOURCES]),
	compile:file(?NODE_RESOURCES).
