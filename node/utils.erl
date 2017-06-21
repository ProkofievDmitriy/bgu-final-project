-module(utils).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").
-include("./include/macros.hrl").


-export([get_node_number/1, get_node_name/1, get_mac/0, get_ip/0, remove_end_of_line/1, remove_end_of_line/2,
         cut_list_from_delimiter/2, exec_curl/4, grafana_report/3]).

get_node_name(NodeAddress) when is_list(NodeAddress)->
    list_to_atom("node_" ++ NodeAddress);

get_node_name(NodeAddress)->
    list_to_atom("node_" ++ integer_to_list(NodeAddress)).

get_node_number(NodeName) when is_integer(NodeName)->
    NodeName;

get_node_number(NodeName) when is_atom(NodeName)->
    NodeNameAsList = atom_to_list(NodeName),
    get_node_number(NodeNameAsList);

get_node_number(NodeName)->
    ?LOGGER:debug("[~p]: get_node_number NodeName: ~p~n", [?MODULE, NodeName]),
    {Address, []} = string:to_integer(cut_list_from_delimiter(NodeName, 95)), % 95 = "_".
    Address.


get_mac() ->
	M = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep -o -E '([[:xdigit:]]{1,2}:){5}[[:xdigit:]]{1,2}'"),
    ?LOGGER:debug("[~p]: get_mac: mac is:~p~n", [?MODULE, M]),
	remove_end_of_line(M).

get_ip() ->
    I = os:cmd("ifconfig " ++ ?NETWORK_DEVICE ++ " | grep \"inet addr\" | cut -d ':' -f 2 | cut -d ' ' -f 1"),
    ?LOGGER:debug("[~p]: get_ip: ip is:~p~n", [?MODULE, I]),
	remove_end_of_line(I).

remove_end_of_line([])-> none;
remove_end_of_line([H|Tail])->
    case Tail == "\n" of
        true -> H;
        _ -> remove_end_of_line([H], Tail)
    end.

remove_end_of_line(Result, [H | []]) -> Result++[H];
remove_end_of_line(Result, [H | Tail]) when Tail =:= "\n" -> Result++[H];
remove_end_of_line(Result, [H | Tail]) -> remove_end_of_line(Result ++ [H], Tail).

cut_list_from_delimiter([], _Delimiter)-> [];
cut_list_from_delimiter([H|Tail], Delimiter)->
    ?LOGGER:preciseDebug("[~p]: cut_list_from_delimiter H: ~p, Tail: ~p, Delimiter: ~p ~n", [?MODULE, H, Tail, Delimiter]),
    case H == Delimiter of
        true ->
            ?LOGGER:preciseDebug("[~p]: cut_list_from_delimiter Result: ~p~n", [?MODULE, Tail]),
            Tail;
        _ -> cut_list_from_delimiter(Tail, Delimiter)
    end.

grafana_report(Type, GrafanaServerIP, Data)->
    ?LOGGER:preciseDebug("[~p]: grafana_report Type = : ~w ~n",[?MODULE, Type]),
    case  GrafanaServerIP of
        undefined -> ok;
        _ ->
        case Type of
            {management_message,send_message} ->
                utils:exec_curl(GrafanaServerIP, "loadng", "mgmt_msg", "value=1");

            {management_message,received_message} ->
                utils:exec_curl(GrafanaServerIP, "loadng", "mgmt_msg", "value=0");

            {data_message,send_message} ->
                utils:exec_curl(GrafanaServerIP, "loadng", "data_msgs", "value=1");
            {data_message,relay_message} ->
                utils:exec_curl(GrafanaServerIP, "loadng", "data_msgs", "value=2");

            {data_message,received_message} ->
                utils:exec_curl(GrafanaServerIP, "loadng", "data_msgs", "value=0");

            _ -> ok

        end
end.


exec_curl(GrafanaServerIP, DataBase, Table, Values)->
    CurlCmd = "curl -i -XPOST 'http://" ++ GrafanaServerIP ++ ":8086/write?db=" ++ DataBase ++ "' --data-binary '" ++ Table ++" " ++ Values ++ "'",
    ?LOGGER:debug("[~p]: exec_curl CurlCmd = : ~p ~n",[?MODULE, CurlCmd]),
    Result = os:cmd(CurlCmd),
    ?LOGGER:preciseDebug("[~p]: exec_curl Result = : ~p ~n",[?MODULE, Result]).
