%%%-------------------------------------------------------------------
%%% @author admin
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Dec 2015 1:40 AM
%%%-------------------------------------------------------------------
-module('node').
-author("admin").

-behaviour(gen_server).

-define(SERVER, server_port).
-define(Modem, modem,_port).
-define(HyRPL, HyRPL).
-define(NODE, isg_node).


%% API

-export([delete_line_feed/2, get_ip/0, get_mac/0, get_id/0]).
-export([extract_id/1]).

-export([start/0, stop/0]).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


-record(state, {state,file_version, node_name, node_mac_addr, node_ip_addr, node_id , server , modem ,hyRPL, app, ka, server_ref, modem_ref, hyRPL_ref, ka_ref, app_ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
%-spec(start_link() ->

%  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).


start() ->
	gen_server:start_link({local,?NODE }, ?MODULE, [], []), receive odfihjdioh->done end.

stop() ->
	gen_server:call(?NODE, stop).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%-spec(init(Args :: term()) ->
 % {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  %{stop, Reason :: term()} | ignore).
init([]) ->
	io:format("starting node!~n"),
	process_flag(trap_exit, true),
	%extract muchine's parameters (MAC Address, IP Address, Host name & Serial ID number
	{HostName,ID} = get_id(),
	Node_Name = HostName,
	MAC_Addr = get_mac(),
	io:format("node_~p: my MAC is:~p.~n", [ID,  MAC_Addr]),
	IP_Addr = get_ip(),
	Version = get_file_version(),
	%init server_port
	{ok, Pid_server} = server_port:start_link(ID, MAC_Addr, IP_Addr, Node_Name),
	Server_Ref = erlang:monitor(process, Pid_server),
	io:format("local server port initiated and monitored!~n"),
	
	%initiate HyRPL FSM 
	Atom_ID = list_to_atom(integer_to_list(ID)),
	Pid_HyRPL = hyRPL:start(Atom_ID),
	HyRPL_Ref = erlang:monitor(process, Pid_HyRPL),
	io:format("HyRPL initiated and monitored! pid is:~p, Ref:~p~n",[Pid_HyRPL, HyRPL_Ref]),

	%initiate modem_port module
	Pid_modem = modem_port:start(),
	Modem_Ref = erlang:monitor(process, Pid_modem),
	io:format("modem_port initiated and monitored!~n"),

	Pid_app = app:start(ID),
	App_Ref = erlang:monitor(process, Pid_app),
	io:format("App initiated and monitored!~n"),
	
	%Keep Alive init
	KA_Pid = spawn( fun()-> keepAlive_timer() end),
	KA_Ref = erlang:monitor(process, KA_Pid),
	io:format("keep alive initiated and monitored!~n"),

  {ok, #state{state = active, file_version = Version, node_name = Node_Name, node_mac_addr = MAC_Addr, node_ip_addr = IP_Addr,
   node_id = ID,
	 server = Pid_server, server_ref = Server_Ref,  
	 modem = Pid_modem, modem_ref = Modem_Ref,
	 hyRPL = Pid_HyRPL, hyRPL_ref = HyRPL_Ref,
	 app = Pid_app, app_ref = App_Ref,
	 ka = KA_Pid, ka_ref = KA_Ref}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

%call for termination of node gen server
handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
 

handle_cast(resume , #state{ state = S}  = State) when S ==active  ->  io:format("~n~nnode:handle_cast: resume: got resume at active state~n~n"), {noreply, State};
handle_cast(resume , State) ->
	io:format("node:handle_cast: resume:!@#$$^%&^()&*+_*^*$^@$!~n"),
	HyRPL_Pid = hyRPL:start(list_to_atom(integer_to_list(State#state.node_id))),
	io:format("node:handle_cast: pause:after HyRpl:start()~n"),
	HyRPL_ref = erlang:monitor(process, HyRPL_Pid),
	APP_Pid = app:start(State#state.node_id),
	APP_ref = erlang:monitor(process, APP_Pid),
	gen_server:call(State#state.server, resume_notification),
  {noreply, #state{state = active,file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
   node_id = State#state.node_id, 
   server = State#state.server, server_ref = State#state.server_ref, 
   modem = State#state.modem,	modem_ref = State#state.modem_ref, 
   hyRPL = HyRPL_Pid, hyRPL_ref = HyRPL_ref,
   app = APP_Pid, app_ref = APP_ref,
	 ka = State#state.ka, ka_ref = State#state.ka_ref}};

handle_cast(pause , #state{state = S} = State) when S == non_active -> io:format("~n~nnode:handle_cast: pause: got pause at non-active state~n~n"), ignore, {noreply, State};
handle_cast(pause , State) ->
	io:format("node:handle_cast: pause:!@#$$^%&^()&*+_*^*$^@$!~n"),
	hyRPL:stop(),
	io:format("node:handle_cast: pause:after HyRpl:STOP()~n"),
	erlang:demonitor(State#state.hyRPL_ref),
	app:stop(),
	erlang:demonitor(State#state.app_ref),
	Ans = gen_server:call(State#state.server, termination_notification),
	io:format("node:handle_cast: pause: Ans of call termination_notification is:~p~n", [Ans]),
  {noreply, #state{state = non_active,file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
   node_id = State#state.node_id, 
   server = State#state.server, server_ref = State#state.server_ref, 
   modem = State#state.modem,	modem_ref = State#state.modem_ref, 
   hyRPL = none, hyRPL_ref = none,
   app = none, app_ref = none,
	 ka = State#state.ka, ka_ref = State#state.ka_ref}};

  
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
 % {noreply, NewState :: #state{}} |
  %{noreply, NewState :: #state{}, timeout() | hibernate} |
  %{stop, Reason :: term(), NewState :: #state{}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%handels all errors from monitored processes%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%case server gate crashed. restart it
handle_info( {'DOWN', Ref , process, _Pid, _Reason}, #state{server_ref = Ref} = State) ->
	io:format("node: serer_port crashed. re-start it~n"),
	{ok, Pid_server} = server_port:start_link(State#state.node_mac_addr, State#state.node_name),
	Server_Ref = erlang:monitor(process, Pid_server),
	%return%	
  {noreply, #state{state=State#state.state, file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
  node_id = State#state.node_id,
	server = Pid_server, server_ref = Server_Ref,  
	modem = State#state.modem, modem_ref = State#state.modem_ref,
	hyRPL = State#state.hyRPL, hyRPL_ref = State#state.hyRPL_ref,
	app = State#state.app, app_ref = State#state.app_ref,
	ka = State#state.ka, ka_ref = State#state.ka_ref}};

%case Modem module crashed. restart it
handle_info( {'DOWN', Ref , process, _Pid, _Reason}, #state{modem_ref = Ref} = State) ->
	io:format("node: modem_port crashed. re-start it~n"),
	Pid_modem = modem_port:start(),
	Modem_Ref = erlang:monitor(process, Pid_modem),
  %return%
  {noreply, #state{state=State#state.state, file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
  node_id = State#state.node_id,
	server = State#state.server, server_ref = State#state.server_ref,  
	modem = Pid_modem, modem_ref = Modem_Ref,
	hyRPL = State#state.hyRPL, hyRPL_ref = State#state.hyRPL_ref,
	app = State#state.app, app_ref = State#state.app_ref,
	ka = State#state.ka, ka_ref = State#state.ka_ref}};

%case Modem module was shutdown.
handle_info( {'DOWN', Ref , process, _Pid, shutdown}, #state{modem_ref = Ref} = State) ->
	io:format("node: modem_port terminated~n"),
	%return%
	{noreply, #state{state=State#state.state, file_version = State#state.file_version, node_name = State#state.node_name, 
	node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
	node_id = State#state.node_id,
	server = State#state.server, server_ref = State#state.server_ref,  
	modem = not_define, modem_ref = not_define,
	hyRPL = State#state.hyRPL, hyRPL_ref = State#state.hyRPL_ref,
	app = State#state.app, app_ref = State#state.app_ref,
	ka = State#state.ka, ka_ref = State#state.ka_ref}};

%case HyRPL FSM crashed. restart it
handle_info( {'DOWN', Ref , process, _Pid, Reason}, #state{hyRPL_ref = Ref} = State)  ->
	io:format("node: *****~n*****~n*****~nHyRPL crashed. reason is: ~p~n. re-start it~n",[Reason]),
	%erlang:demonitor(Ref),
	Pid_HyRPL = hyRPL:start(list_to_atom(integer_to_list(State#state.node_id))),
	io:format("node: pass hyrpl:start~n"),
	NewHyRPL_Ref = erlang:monitor(process, Pid_HyRPL),
	io:format("node: pass monitor~n"),
  %return%
  {noreply, #state{state=State#state.state, file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
  node_id = State#state.node_id,
	server = State#state.server, server_ref = State#state.server_ref,  
	modem = State#state.modem, modem_ref = State#state.modem_ref,
	hyRPL = Pid_HyRPL, hyRPL_ref = NewHyRPL_Ref,
	app = State#state.app, app_ref = State#state.app_ref,
	ka = State#state.ka, ka_ref = State#state.ka_ref}};

	%case Application crashed. restart it
handle_info( {'DOWN', Ref , process, _Pid, Reason}, #state{app_ref = Ref} = State)  ->
	io:format("node: *****~n*****~n*****~nAPP crashed. reason is: ~p~n. re-start it~n",[Reason]),
	Pid_app = app:start(State#state.node_name),
	App_Ref = erlang:monitor(process, Pid_app),
  %return%
  {noreply, #state{state=State#state.state, file_version = State#state.file_version, node_name = State#state.node_name, 
  node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
  node_id = State#state.node_id,
	server = State#state.server, server_ref = State#state.server_ref,  
	modem = State#state.modem, modem_ref = State#state.modem_ref,
	hyRPL = State#state.hyRPL, hyRPL_ref = State#state.hyRPL_ref,
	app = Pid_app, app_ref = App_Ref,
	ka = State#state.ka, ka_ref = State#state.ka_ref}};

handle_info( {'EXIT',From,  Reason, X }, State)  ->
	io:format("~n~n~nnode:hhhhhhhhhhhhhhhhhhhhhhhandle_info:got Exit msg from: ~p reason:~p~n~n~n", [From, Reason]),
	{noreply, State};

handle_info( {'EXIT',From,  Reason }, State)  ->
	io:format("~n~n~nnode:handle_info:got Exit msg from: ~p reason:~p~n~n~n", [From, Reason]),
	{noreply, State}.
 


%case Keep Alive process  crashed. restart it
% handle_info( {'DOWN', Ref , process, _Pid, error}, #state{ka_ref = Ref} = State) ->
% 		io:format("node: KA crashed. re-start it~n"),
% 	KA_Pid = spawn( fun()-> keepAlive_timer() end),
% 	KA_Ref = erlang:monitor(process, KA_Pid),
%   {noreply, #state{file_version = State#state.file_version, node_name = State#state.node_name, 
%   %return%
%   node_mac_addr = State#state.node_mac_addr, node_ip_addr = State#state.node_ip_addr,
%   node_id = State#state.node_id,
% 	server = State#state.server, server_ref = State#state.server_ref,  
% 	modem = State#state.modem, modem_ref = State#state.modem_ref,
% 	hyRPL = State#state.hyRPL, hyRPL_ref = State#state.hyRPL_ref,
% 	ka = KA_Pid, ka_ref = KA_Ref}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(_Reason, State) ->
	io:format("node: terminating!!!~n"),

	app:stop(),
	io:format("node:application terminated!~n"),

	hyRPL:send_message_Fsm(stop),
	timer:sleep(500),
	io:format("node: terminated hyRPL!!!~n"),

	modem_port:stop(),
	timer:sleep(500),
	io:format("node: terminated modem_port!!!~n"),

	gen_server:call(State#state.server, stop),
	timer:sleep(1000),
	io:format("node: terminated server_port!!!~n"),

	%temp
	State#state.ka ! stop,
	timer:sleep(1000),
	io:format("node: terminated ka!!!~n"),
	io:format("node: alllllll is gone~n"),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

delete_line_feed(M1, [M | Rest]) when Rest =:= "\n" -> M1++[M];
delete_line_feed(M1, [M | Rest]) -> M2 = M1 ++ [M], delete_line_feed(M2, Rest). 


get_mac() -> 
	M = os:cmd("ifconfig wlan0 | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
	delete_line_feed([], M).
get_ip() -> I = os:cmd("ifconfig wlan0 | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"), 
	delete_line_feed([], I).


%extract_id([H|Rest]) when Rest =:= "\n" orelse Rest =:= [] -> H;
%extract_id([_H|Rest]) -> extract_id(Rest).


extract_id2([H|Rest], L) when H =:= 10 -> L;
extract_id2([H|Rest], L) -> L1 = L ++ [H], extract_id2( Rest, L1 ).


extract_id([H|Rest]) when H =:= 95 -> extract_id2(Rest, []);
extract_id([H|Rest]) ->  extract_id(Rest).

get_id() ->
	Host = os:cmd("hostname"),
	io:format("at get_id: hastname is:~p~n", [Host]),
	A = extract_id(Host),
	{delete_line_feed([],Host), list_to_integer(A)}.

	
get_file_version() -> todo, 1.

%awake every 30 minuts, and sends server port a keep alive notification
keepAlive_timer() -> 
	receive
		stop -> bye
	after 1000*60*30 -> gen_server:cast(server, keep_alive), keepAlive_timer()
	end.


