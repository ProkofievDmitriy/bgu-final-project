-module(node_control_interface).

-define(DC_NAME, node_1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([update_configuration/2, initiate_transaction/3, reset_node/1, update_nodes_to_filter/2, start_application/1, disable/1, enable/1]).
-export([configuration_updated_from_gui/1,routing_tables_cleared_from_gui/0,stations_removed_from_gui/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      node controll interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_configuration(all, _)->
    io:format("update_configuration to all INGNORED.... ");

update_configuration(NodeName, OptionsList)->
    gen_server:cast(global:whereis_name(NodeName), {update_configuration, OptionsList}).


initiate_transaction(all, _ , _)->
    io:format("initiate_transaction not matched .... INGNORED~n");

initiate_transaction(_, '', _)->
    io:format("initiate_transaction not matched .... INGNORED~n");

initiate_transaction(NodeName, Destination, Data) when is_atom(Destination) andalso is_atom(NodeName)->
    io:format("initiate_transaction Sending from ~p, to ~p, Data: ~p ~n", [NodeName, Destination, Data]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, {initiate_transaction, {Destination, Data}}).


update_nodes_to_filter(all, _)->
    io:format("update_nodes_to_filter to all INGNORED.... ");

update_nodes_to_filter(NodeName, NodesToFilterList)->
    io:format("update_nodes_to_filter to ~p, NodesToFilterList: ~p ~n", [NodeName, NodesToFilterList]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, {update_nodes_to_filter, NodesToFilterList}).


start_application(NodeName)->
    io:format("sending start_application to ~p~n", [NodeName]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, {start_application}).

disable(NodeName)->
    io:format("REMOVE NODE: sending disable to ~p~n", [NodeName]),
    A = global:whereis_name(NodeName),
    stations_removed_from_gui([NodeName]),
    gen_server:cast(A, disable).

enable(NodeName)->
    io:format("sending enable to ~p~n", [NodeName]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, enable).

reset_node(NodeName)->
    io:format("~nAAAA3: ~p~n",[NodeName]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, {reset_node}),
    io:format("BBBB: ~p~n~n",[A]).


%% ---------------------------------------------------------------------------------------------------------
%%                    Server -> Application notifications (for Liya to fill)
%% ---------------------------------------------------------------------------------------------------------


% in case a configuration update was issued from gui interface, the server should notify the app by calling this function
% ListOfNodesAndMediums - each element is a tuple {NodeName, MediumType}, MediumType can be plc or rf or dual
configuration_updated_from_gui (ListOfNodesAndMediums) ->
    io:format("configuration_updated_from_gui to ~p, ListOfNodesAndMediums: ~p ~n", [?DC_NAME, ListOfNodesAndMediums]),
    A = global:whereis_name(?DC_NAME),
    gen_server:cast(A, {configuration_updated_from_gui, ListOfNodesAndMediums}).

% in case routing tables were cleared from gui interface, the server should notify the app by calling this function
routing_tables_cleared_from_gui() ->
    io:format("routing_tables_cleared_from_gui to ~p ~n", [?DC_NAME]),
    A = global:whereis_name(?DC_NAME),
    gen_server:cast(A, {routing_tables_cleared_from_gui}).

stations_removed_from_gui(ListOfNodes) ->
    io:format("stations_removed_from_gui to ~p, ListOfNodes: ~p ~n", [?DC_NAME, ListOfNodes]),
    A = global:whereis_name(?DC_NAME),
    gen_server:cast(A, {stations_removed_from_gui, ListOfNodes}).
