%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%

-module(db_tests).
-export([start/0, stop_all/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    net_kernel:connect_node('G@127.0.0.1'),
    P1 = start_node(node_1),
    receive after 50-> ok end,
    P2 = start_node(node_2),
    receive after 50-> ok end,
    P3 = start_node(node_3),
    receive after 50-> ok end,
    P4 = start_node(node_4),
    receive after 1000-> ok end,
    [P1,P2,P3,P4].

stop_all([]) -> ok;
stop_all([P|List])->
    P!stop,
    stop_all(List).

start_node(Node)->
    %N = atom_to_list(Node),

    Pid = spawn(fun() -> run_node(Node,1) end),


    Pid. 

%% {otherNode,plc/rf/{nei,thru}}
run_node(Node,Key)->
    io:format("~p is running~n", [Node]),
    Rand = rand:uniform(4),
    case Rand of
        1 -> stats_server_interface:received_management_message(Node, Node, Key);
        2 -> stats_server_interface:sent_management_message (Node, Node, Key);
        3 -> stats_server_interface:received_data_message (Node, Node, Key);
        4 -> stats_server_interface:sent_data_message (Node, Node, Key)
    end,

    receive 
    after 10000->
        run_node(Node, Key + 1)
    end.

