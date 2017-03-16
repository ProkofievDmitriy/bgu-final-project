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
-export([new_node_is_up/2, node_is_down/1, update_state/4, printNodes/0, report/1]).



%%%===================================================================
%%% API
%%%===================================================================

%**************************************************************************************
%**********************   Data Server  Updated  *******************************
%**************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  New node is up  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%+

report({Type, Data})->
	case Type of
		node_state -> NodeName = proplists:get_value(node_name, Data),
			new_node_is_up(NodeName, Data);
		_ ->
			io:format("Ignoring report type:  ~p, Data : ~w~n",[Type, Data])
		end.



new_node_is_up (NodeName, Data) ->
	io:format("New node is up with id ~p, data: ~w~n",[NodeName, Data]),
	Server = global:whereis_name(loadNGgui),
	io:format("Server ~p~n",[Server]),
	wx_object:cast(Server, {node_is_up, {NodeName, Data}}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Node is down  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
node_is_down (Id) ->
	io:format("Node is down with id ~p~n",[Id]),
		Server = global:whereis_name(loadNGgui),

	wx_object:cast(Server, {node_is_down,{Id}}	).

node_state(NodeName, MedMode,RoutingSet)->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  set_node_state  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
update_state(NodeName, Plc,Rf,RoutingSet)->
	io:format("Node ~p State to: PLC- ~p, RF: ~p~n",[NodeName, Plc,Rf]),
		Server = global:whereis_name(loadNGgui),

	wx_object:cast(Server, {update_state,{NodeName,{{Plc,Rf},RoutingSet}}}	).
%%--------------------------------------------------------------------

%%  ------------------------------------------------------------------
%%	-------------------   server Debug ONLY     ----------------------
%%  ------------------------------------------------------------------

printNodes () ->
	io:format("Print ETS~n"),
		Server = global:whereis_name(loadNGgui),

	gen_server:cast(Server, {printNodes}).
