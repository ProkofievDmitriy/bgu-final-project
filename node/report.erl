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

-record(context, {node_name, data_server_interface, data_server_name, data_server_ip, connected_to_server}).

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
	?LOGGER:debug("[~w]: Starting REPORT with props: ~w~n", [?MODULE, Properties]),
    DataServerInterface = proplists:get_value(data_server_interface, Properties),
    DataServerName = proplists:get_value(data_server_name, Properties),
    DataServerIp = proplists:get_value(data_server_ip, Properties),
    NodeName = proplists:get_value(node_name, Properties),
    {ok, #context{
        node_name = NodeName,
        data_server_interface = DataServerInterface,
        data_server_name = DataServerName,
        data_server_ip = DataServerIp,
        connected_to_server = false
         }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CALL's synchronprepare_message_dataous requests, reply is needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~w]: STUB Handle CALL Request(~w) from ~w, Context: ~w~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE CAST's a-synchronous requests
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_cast({report, {Type, DataList}}, #context{connected_to_server = true} = Context) ->
    spawn(fun()-> utils:grafana_report(Type, Context#context.data_server_ip, DataList) end),
    ReportMessageData = prepare_message_data(DataList, Context),
    ServerModuleInterface = Context#context.data_server_interface,
    ?LOGGER:debug("[~w]: REPORT, ServerModuleInterface: ~w, Data: ~w~n", [?MODULE, ServerModuleInterface, ReportMessageData]),
    ReportResult = ServerModuleInterface:report(Type, ReportMessageData),
    case ReportResult of
        {ok, _} -> {noreply, Context};
        {error, Throw} ->
            ?LOGGER:warn("[~w]: ServerModuleInterface: ~w unavailable: ~w.~n", [?MODULE, ServerModuleInterface, Throw]),
            {noreply, Context#context{connected_to_server = false}};
        Else ->
            ?LOGGER:critical("[~w]: ServerModuleInterface: ~w UNXEPECTED RESULT: ~w.~n", [?MODULE, ServerModuleInterface, Else]),
            {noreply, Context#context{connected_to_server = false}}
    end;

handle_cast({report, {Type, DataList} }, #context{connected_to_server = false} = Context) ->
    spawn(fun()-> utils:grafana_report(Type, Context#context.data_server_ip, DataList) end),
    ?LOGGER:warn("[~w]: REPORT IGNORED - NOT CONNECTED TO DATA SERVER . ~w ~n", [?MODULE, {Type, DataList}]),
    connect_to_data_server(),
    {noreply, Context};


handle_cast(connect_to_data_server, #context{connected_to_server = false} = Context) ->
%TODO Pattern match in function definition to only not connected to server state
    ?LOGGER:debug("[~w]: Handle ResultCAST Request(connect_to_data_server) ~n", [?MODULE]),
    ServerNodeName = list_to_atom(atom_to_list(Context#context.data_server_name) ++ "@" ++ Context#context.data_server_ip),
    test_connection(ServerNodeName),
    Ans = net_kernel:connect_node(ServerNodeName),
    timer:sleep(1000),
    case Ans of
        true ->
            ?LOGGER:debug("[~w]: connected to server: ~w, erlang-wise (Ans is true)~n", [?MODULE, ServerNodeName]),
            NewContext = Context#context{connected_to_server = true},
            {noreply, NewContext};
        Else ->
            ?LOGGER:err("[~w]: ERROR: ~w, on connection to server: ~w~n", [?MODULE, Else, ServerNodeName]),
            timer:sleep(10000),
            connect_to_data_server(),
            {noreply, Context}
    end;

handle_cast(connect_to_data_server, #context{connected_to_server = true} = Context) ->
    ?LOGGER:preciseDebug("[~w]: connect_to_data_server IGNORED - ALLREADY CONNECTED~n", [?MODULE]),
    {noreply, Context};




handle_cast(Request, Context) ->
    ?LOGGER:debug("[~w]: STUB Handle CAST Request(~w), Context: ~w ~n", [?MODULE, Request, Context]),
    {noreply, Context}.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   HANDLE INFO's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_info(Request, Context)  ->
    ?LOGGER:debug("[~w]: STUB Handle INFO Request(~w), Context: ~w~n", [?MODULE, Request, Context]),
	{noreply, Context}.



terminate(Reason, Context) ->
    ?LOGGER:debug("[~w]: STUB terminating, reason ~w, state ~w~n", [?MODULE, Reason, Context]),
    ok.

code_change(_OldVsn, Context, _Extra) -> {ok, Context}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_message(Type, DataList , Context)->
    {Type, prepare_message_data(DataList, Context)}.

prepare_message_data(DataList , Context)->
    [{node_name, list_to_atom(Context#context.node_name)}|DataList].

test_connection(Address)->
   case net_adm:ping(Address) of
     pong-> ?LOGGER:debug("[~w]: Node ~w is UP ~n", [?MODULE, Address]);
     pang-> ?LOGGER:debug("[~w]: Can't reach node ~w~n", [?MODULE, Address]);
     Other-> ?LOGGER:debug("[~w]: ERROR :  ~w~n", [?MODULE, Other])
    end.
