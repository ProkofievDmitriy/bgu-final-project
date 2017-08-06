-module(node_control_interface).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([update_configuration/2, initiate_transaction/3, reset_node/1, update_nodes_to_filter/2]).

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
    gen_server:cast(NodeName, {update_nodes_to_filter, NodesToFilterList}).

reset_node(NodeName)->
    io:format("~nAAAA3: ~p~n",[NodeName]),
    A = global:whereis_name(NodeName),
    gen_server:cast(A, {reset_node}),
    io:format("BBBB: ~p~n~n",[A]).
