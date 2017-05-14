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
-compile(export_all).

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
    P1!plc,
    P2!rf,
    P3!plc_rf,
    P4!plc_rf,

    receive after 1000-> ok end,
    P1! {routinSet,[
            #routing_set_entry{dest_addr = 4, next_addr = 4, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 4, next_addr = 2, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1}
            ]},
    P2! {routinSet,[
            #routing_set_entry{dest_addr = 1, next_addr = 4, medium = 1, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 3, next_addr = 3, medium = 1, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 4, next_addr = 4, medium = 1, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1}

            ]},
    P3! {routinSet,[
            #routing_set_entry{dest_addr = 1, next_addr = 4, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 2, next_addr = 2, medium = 1, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 4, next_addr = 4, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1}

            ]},
    P4! {routinSet,[
            #routing_set_entry{dest_addr = 1, next_addr = 1, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 2, next_addr = 2, medium = 1, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1},
            #routing_set_entry{dest_addr = 3, next_addr = 3, medium = 2, hop_count = 1, r_seq_number = 1, bidirectional = 0, valid_time = 100, valid = 1}

            ]},
    [P1,P2,P3,P4].

stop_all([]) -> ok;
stop_all([P|List])->
    P!stop,
    stop_all(List).

start_node(Node)->
    %N = atom_to_list(Node),
    stats_server_interface:node_is_up(Node,[{medium_mode,{0,0}},{routing_set,[]}]),

    Pid = spawn(fun() -> run_node(Node,0,0,[]) end),


    Pid. 

%% {otherNode,plc/rf/{nei,thru}}
run_node(Node,PLC,RF,RoutingSet)->
    io:format("~p sending, PLC: ~p, RF: ~p RoutingSet: ~p~n", [Node,PLC,RF,RoutingSet]),
    stats_server_interface:node_is_up(Node,[{medium_mode,{PLC,RF}},{routing_set,RoutingSet}]),

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

