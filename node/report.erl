-module(report).

-behaviour(gen_server).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start/1, stop/0, connect_to_data_server/0, report/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(context, {node_name, data_server_name, data_server_ip, connected_to_server}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Params) ->
    {ok,PID} = gen_server:start({local, ?MODULE }, ?MODULE, Params, []),
    PID.

stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


report(Type, Message)-> gen_server:cast(?MODULE, {report, {Type, Message}}).
connect_to_data_server() -> gen_server:cast(?MODULE, connect_to_data_server).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Properties) ->
	?LOGGER:debug("[~p]: Starting REPORT with props: ~w~n", [?MODULE, Properties]),
    DataServerName = proplists:get_value(data_server_name, Properties),
    DataServerIp = proplists:get_value(data_server_ip, Properties),
    NodeName = proplists:get_value(node_name, Properties),
    {ok, #context{
        node_name = NodeName,
        data_server_name = DataServerName,
        data_server_ip = DataServerIp,
        connected_to_server = false
         }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CALL Request(~w) from ~p, Context: ~p~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({report, {Type, DataList}}, #context{connected_to_server = true} = Context) ->
    ReportMessage = prepare_message(Type, DataList, Context),
    ServerModuleInterface = Context#context.data_server_name,
    ?LOGGER:debug("[~p]: Handle CAST Request(report), ServerModuleInterface: ~p, Message: ~w~n", [?MODULE, ServerModuleInterface, ReportMessage]),
    try ServerModuleInterface:report(ReportMessage) of
        _ -> {noreply, Context}
    catch
        Throw->
            ?LOGGER:warn("[~p]: ServerModuleInterface: ~w unavailable: ~p.~n", [?MODULE, ServerModuleInterface, Throw]),
            {noreply, Context#context{connected_to_server = false}}
    end;

handle_cast({report, _ }, #context{connected_to_server = false} = Context) ->
    ?LOGGER:warn("[~p]: REPORT IGNORED - NOT CONNECTED TO DATA SERVER .~n", [?MODULE]),
    connect_to_data_server(),
    {noreply, Context};


handle_cast(connect_to_data_server, #context{connected_to_server = false} = Context) ->
%TODO Pattern match in function definition to only not connected to server state
    ?LOGGER:debug("[~p]: Handle CAST Request(connect_to_data_server) ~n", [?MODULE]),
    ServerNodeName = list_to_atom(atom_to_list(Context#context.data_server_name) ++ "@" ++ Context#context.data_server_ip),
    test_connection(ServerNodeName),
    Ans = net_kernel:connect_node(ServerNodeName),
    timer:sleep(1000),
    case Ans of
        true ->
            ?LOGGER:debug("[~p]: connected to server: ~p, erlang-wise (Ans is true)~n", [?MODULE, ServerNodeName]),
            NewContext = Context#context{connected_to_server = true},
            {noreply, NewContext};
        Else ->
%    		?LOGGER:err("[~p]: ERROR: ~p, on connection to server: ~p~n", [?MODULE, Else, ServerNodeName]),
            ?LOGGER:err("[~p]: ERROR: ~p, on connection to server: ~p~n", [?MODULE, Else, ServerNodeName]),
            timer:sleep(10000),
            connect_to_data_server(),
            {noreply, Context}
    end;

handle_cast(connect_to_data_server, #context{connected_to_server = true} = Context) ->
    ?LOGGER:debug("[~p]: Handle CAST Request(connect_to_data_server) IGNORED - ALREADY CONNECTED~n", [?MODULE]),
    {noreply, Context};




handle_cast(Request, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CAST Request(~w), Context: ~p ~n", [?MODULE, Request, Context]),
    {noreply, Context}.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info(Request, Context)  ->
    ?LOGGER:debug("[~p]: STUB Handle INFO Request(~w), Context: ~p~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    ?LOGGER:debug("[~p]: STUB terminating, reason ~p, state ~p~n", [?MODULE, Reason, Context]),
    ok.

code_change(_OldVsn, Context, _Extra) -> {ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_message(Type, DataList , Context)->
    {Type, [{node_name, list_to_atom(Context#context.node_name)}|DataList]}.


test_connection(Address)->
   case net_adm:ping(Address) of
     pong-> ?LOGGER:debug("[~p]: Node ~p is UP ~n", [?MODULE, Address]);
     pang-> ?LOGGER:debug("[~p]: Can't reach node ~p~n", [?MODULE, Address]);
     Other-> ?LOGGER:debug("[~p]: ERROR :  ~p~n", [?MODULE, Other])
    end.
