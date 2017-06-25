-module(protocol_interface).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-define(PROTOCOL_NAME, protocol).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/0, send/2, hand_shake/2, hand_shake/1, send_data_request/2, send_data_reply/2, update_configuration/1, reset/0, get_status/0]).

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
send(Destination, Data) when is_binary(Data)->
    gen_server:call(?PROTOCOL_NAME, {data_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT);

send(Destination, Data)->
    send(Destination, term_to_binary(Data)).


send_data_request(Destination, Data)->
    gen_server:call(?PROTOCOL_NAME, {data_request_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT).

send_data_reply(Destination, Data)->
    gen_server:call(?PROTOCOL_NAME, {data_reply_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT).


%essential for protocol to be aware of upper apllication layer
hand_shake(ApplicationPid, Timeout) ->  % App_type might be dc or sem
   gen_server:call(?PROTOCOL_NAME, {hand_shake, ApplicationPid} , Timeout).

hand_shake(ApplicationPid)->
    gen_server:call(?PROTOCOL_NAME, {hand_shake, ApplicationPid}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




update_configuration(OptionsList)->
    gen_server:cast(?PROTOCOL_NAME, {update_configuration, OptionsList}).

reset()->
    gen_server:call(?PROTOCOL_NAME, reset).

% return list of tuples [{destination, Destination}, {next_address, NextAddress}, {medium, Medium}]
get_status()->
    gen_server:call(?PROTOCOL_NAME, get_status , 60000).
