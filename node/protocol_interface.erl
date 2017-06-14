-module(protocol_interface).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-define(PROTOCOL_NAME, protocol).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/0, send/2, hand_shake/2, send_data_request/2, send_data_reply/2, update_configuration/1, reset/0, get_status/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(ProtocolModule, Properties) ->
    ?LOGGER:debug("[~p]: Starting protocol: ~p, with props: ~w~n", [?MODULE, ProtocolModule, Properties]),
    Timeout = proplists:get_value(timeout, Properties),
    {ok,PID} = gen_server:start_link({local, ?PROTOCOL_NAME}, ProtocolModule, Properties, [{timeout, Timeout}]),
    PID.

stop() ->
    gen_server:call(?PROTOCOL_NAME, stop).


%%%%%%%%%%%%%%%%%%%%%%%% APLICATION INSTERFACE
send(Destination, Data)->
    gen_server:call(?PROTOCOL_NAME, {data_message, {extract_address(Destination), Data}}).


send_data_request(Destination, Data)->
    gen_server:call(?PROTOCOL_NAME, {data_request_message, {extract_address(Destination), Data}}).


send_data_reply(Destination, Data)->
    gen_server:call(?PROTOCOL_NAME, {data_reply_message, {extract_address(Destination), Data}}).


%essential for protocol to be aware of upper apllication layer
hand_shake(ApplicationPid, Timeout) ->  % App_type might be dc or sem
   gen_server:call(?PROTOCOL_NAME, {hand_shake, ApplicationPid} , Timeout).

extract_address(NodeNameAtom)->
%TODO implement generic !!!!!
    case NodeNameAtom of
        node_15 -> 15;
        node_14 -> 14;
        node_13 -> 13;
        node_12 -> 12;
        node_11 -> 11;
        node_10 -> 10;
        node_9 -> 9;
        node_8 -> 8;
        node_7 -> 7;
        node_6 -> 6;
        node_5 -> 5;
        node_4 -> 4;
        node_3 -> 3;
        node_2 -> 2;
        node_1 -> 1
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




update_configuration(OptionsList)->
    gen_server:call(?PROTOCOL_NAME, {update_configuration, OptionsList}).

reset()->
    gen_server:call(?PROTOCOL_NAME, {reset}).

% return list of tuples [{destination, Destination}, {next_address, NextAddress}, {medium, Medium}]
get_status()->
    gen_server:call(?PROTOCOL_NAME, get_status).
