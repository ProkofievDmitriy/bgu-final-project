%%%-------------------------------------------------------------------
%%% @author Deddy Zagury
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2017
%%%-------------------------------------------------------------------
-module(loadNG_server_interface).
-author("Deddy Zagury").


-define(MANAGMENT_SERVER, loadNGgui).

%% API
-export([new_node_is_up/3, node_is_down/1, set_node_state/3, printNodes/0]).



%%%===================================================================
%%% API
%%%===================================================================

%**************************************************************************************
%**********************   Data Server  Updated  *******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  New node is up  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%+
new_node_is_up (NodeName) ->
	io:format("New node is up with id ~p~n",[Id]),
	Server = global:whereis_name(loadNGgui),
	io:format("Server ~p~n",[Server]),
	wx_object:cast(Server, {node_is_up,{Id,RF,PLC}}	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node is down  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_is_down (Id) ->
	io:format("Node is down with id ~p~n",[Id]),
		Server = global:whereis_name(loadNGgui),

	wx_object:cast({global, ?MANAGMENT_SERVER}, {node_is_down,{Id}}	).

node_state(NodeName, MedMode,RoutingSet)->
	ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  set_node_state  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_node_state (Id,RF,PLC) ->
	io:format("Node ~p changed state to: RF- ~p, PLC: ~p~n",[Id,RF,PLC]),
		Server = global:whereis_name(loadNGgui),

	wx_object:cast({global, ?MANAGMENT_SERVER}, {change_state,{Id,RF,PLC}}	).
%%%%%%%%%%%%%%%

update_route()
%%--------------------------------------------------------------------

%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printNodes () ->
	io:format("Print ETS~n"),
		Server = global:whereis_name(loadNGgui),

	gen_server:cast(Server, {printNodes}).

