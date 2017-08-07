-module(protocol_interface).
-include("./include/properties.hrl").
-include("./include/macros.hrl").
-include("./include/vcb.hrl").

-define(PROTOCOL_NAME, protocol).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/0, send/2, hand_shake/2, hand_shake/1, send_data_request/2, send_data_reply/2, update_configuration/1, reset/0, get_status/0, update_nodes_to_filter/1]).

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
    {ok,PID} = gen_server:start({local, ?PROTOCOL_NAME}, ProtocolModule, Properties, [{timeout, Timeout}]),
    PID.

stop() ->
    gen_server:call(?PROTOCOL_NAME, stop).


%%%%%%%%%%%%%%%%%%%%%%%% APLICATION INSTERFACE
send(Destination, Data) when is_binary(Data)->
    % gen_server:call(?PROTOCOL_NAME, {data_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT);
    handle_send_message(data_message, Destination, Data);

send(Destination, Data)->
    send(Destination, term_to_binary(Data)).


send_data_request(Destination, Data)->
    % gen_server:call(?PROTOCOL_NAME, {data_request_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT).
    handle_send_message(data_request_message, Destination, Data).

send_data_reply(Destination, Data)->
    % gen_server:call(?PROTOCOL_NAME, {data_reply_message, {utils:get_node_number(Destination), Data}}, ?TIMEOUT).
    handle_send_message(data_reply_message, Destination, Data).


handle_send_message(Type, Destination, Data)->
    gen_server:cast(?PROTOCOL_NAME, {Type, {utils:get_node_number(Destination), Data, self()}}),
    StartTime = utils:get_current_millis(),
    Result = receive
               {ok , sent} -> {ok , sent};
               {error, Error} ->
                   {error, Error}
               after 2 * ?NET_TRAVERSAL_TIME ->
               ?LOGGER:debug("[~p]: Send ASYNCH ~p after timeout to ~p.~n", [?MODULE, Type, Destination]),
               ResultSyncSend = gen_server:call(?PROTOCOL_NAME, {Type, {utils:get_node_number(Destination), Data}}, ?TIMEOUT),
               case ResultSyncSend of
                   {ok , sent} -> {ok, sent};
                   _ ->
                       ?LOGGER:err("[~p]: send TIMEOUT EXCEEDED : ~p.~n", [?MODULE, utils:get_current_millis() - StartTime]),
                      {error, timeout_exceeded}
               end
           end,
    ?LOGGER:info("[~p]: Send ~p to ~p , Call Result: ~p.~n", [?MODULE, Type, Destination, Result]),
    Result.




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

update_nodes_to_filter(NodesToFilter)->
    gen_server:call(?PROTOCOL_NAME, {update_nodes_to_filter, NodesToFilter}).

% return list of tuples [{destination, Destination}, {next_address, NextAddress}, {medium, Medium}]
get_status()->
    StartTime = utils:get_current_millis(),
    Status = (catch gen_server:call(?PROTOCOL_NAME, get_status , 120000)),
    Result = case Status of
          {'EXIT',{timeout,{gen_server,call,_}}} ->
            ?LOGGER:err("[~p]: ERROR TIMEOUT IN get_status", [?MODULE]),
            {error};
          _ -> {ok, Status}
      end,
    ?LOGGER:preciseDebug("[~p]: get_status took ~p ~n", [?MODULE, utils:get_current_millis() - StartTime]),
    Result.
