-module(protocol_interface).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/2, stop/0, send/2, hand_shake/1, send_data_request/1, update_configuration/1, get_status/0]).

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
    {ok,PID} = gen_server:start_link({local, ?MODULE }, ProtocolModule, Properties, [{timeout, Timeout}]),
    PID.

stop() ->
    gen_server:call(?MODULE, stop).

send(Destination, Data)->
    gen_server:call(?MODULE, {data_message, {Destination, Data}}, ?TIMEOUT).

send_data_request(Destination)->
    gen_server:call(?MODULE, {data_request_message, Destination}, ?TIMEOUT).

hand_shake(ApplicationPid) ->
    gen_server:call(?MODULE, {hand_shake, ApplicationPid}).

update_configuration(OptionsList)->
    gen_server:call(?MODULE, {update_configuration, OptionsList}).

get_status()->
    gen_server:call(?MODULE, get_status).
