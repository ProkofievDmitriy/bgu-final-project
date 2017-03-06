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

-module(tests).
-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time, valid}).

%% Client API
-export([start/0, stop_all/1]).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    net_kernel:connect_node('G@127.0.0.1'),
    P1 = start_node(node1),
    receive after 50-> ok end,
    P2 = start_node(node2),
    receive after 50-> ok end,
    P3 = start_node(node3),
    receive after 50-> ok end,
    P4 = start_node(node4),
    receive after 1000-> ok end,
    P4!plc_rf,
    receive after 1000-> ok end,
    P4! {routinSet,[
            #routing_set_entry{dest_addr = node1, next_addr = node1, medium = plc, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = node2, next_addr = node2, medium = rf, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = node3, next_addr = node2, medium = other, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1}

            ]},
    [P1,P2,P3,P4].

stop_all([]) -> ok;
stop_all([P|List])->
    P!stop,
    stop_all(List).

start_node(Node)->
    N = atom_to_list(Node),
    loadNG_server_interface:new_node_is_up(N),

    Pid = spawn(fun() -> run_node(N,0,0,[]) end),


    Pid. 

%% {otherNode,plc/rf/{nei,thru}}
run_node(Node,PLC,RF,RoutingSet)->
    io:format("~p sending, PLC: ~p, RF: ~p RoutingSet: ~p~n", [Node,PLC,RF,RoutingSet]),
    loadNG_server_interface:update_state(Node, PLC,RF,RoutingSet),
    receive 
        stop -> ok;
        plc_rf -> run_node(Node,1,1,RoutingSet);
        rf -> run_node(Node,0,1,RoutingSet);
        plc -> run_node(Node,1,0,RoutingSet);
        no -> run_node(Node,0,0,RoutingSet);
        {routinSet,NewRoutingSet} -> run_node(Node,PLC,RF,NewRoutingSet)


    after 1000->
        run_node(Node,PLC,RF,RoutingSet)
    end.