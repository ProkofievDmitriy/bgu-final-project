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
-export([openPrint/1, start/0, copy/1]).
-compile(export_all).


-define( LOG_DIR,"./logger/").
-define( TEMP_DETS_FILE_DIR, ?LOG_DIR).
-define( TEMP_DETS_FILE, "temp_dets").
%% API

%%%===================================================================
%%% API
%%%===================================================================

-record(counters, {numberOfRelayMsg, numberOfManagementMsgSent, numberOfManagementMsgReceived, numberOfDataMsgSent, numberOfDataMsgReceived}).

parmTest()->
  Counters = #counters{numberOfRelayMsg = 10000, numberOfManagementMsgSent = 10000,
                      numberOfManagementMsgReceived = 10000, numberOfDataMsgSent = 10000, numberOfDataMsgReceived = 10000},
  {StartMegaSecsA, StartSecsA, StartMicroSecsA} = erlang:timestamp(),
  ResA = timeTestA(Counters,1000000,0),
  {EndMegaSecsA, EndSecsA, EndMicroSecsA} = erlang:timestamp(),
  io:format("ResA ~p~n",[ResA]),

  {StartMegaSecsB, StartSecsB, StartMicroSecsB} = erlang:timestamp(),
  ResB = timeTestB(Counters,1000000,0),
  {EndMegaSecsB, EndSecsB, EndMicroSecsB} = erlang:timestamp(),
  io:format("ResB ~p~n",[ResB]),

  DiffA = {EndMegaSecsA - StartMegaSecsA, EndSecsA - StartSecsA, EndMicroSecsA - StartMicroSecsA},
  DiffB = {EndMegaSecsB - StartMegaSecsB, EndSecsB - StartSecsB, EndMicroSecsB - StartMicroSecsB},

  io:format("Test A (take full state) took: ~ps~nTest B (take part state) took: ~ps~n",[DiffA,DiffB]).


timeTestA(_,0,Sum) -> Sum;
timeTestA(Counters,TestNumber,Sum) ->
  NewSum = Sum + testAfun(Counters),
  timeTestA(Counters,TestNumber-1,NewSum).

testAfun(Counters) ->
  Counters#counters.numberOfRelayMsg + Counters#counters.numberOfDataMsgReceived.

timeTestB(_,0,Sum) -> Sum;
timeTestB(Counters,TestNumber,Sum) ->
  NewSum = Sum + testAfun(Counters#counters.numberOfRelayMsg, Counters#counters.numberOfDataMsgReceived),
  timeTestB(Counters,TestNumber-1,NewSum).

testAfun(A,B) ->
  A+B.




time()->
%isg_time:now_now().
calander:now_to_local_time(os:system_time()).



start() ->
    {ok,DB} = dets:open_file(det,[{file, ?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db"},{type,duplicate_bag}]),
    dets:insert(DB,{aaa,aaa}),
    dets:insert(DB,{bbb,aaa1}),
    dets:insert(DB,{bbb,aaa2}),
    printDets(DB, dets:first(DB)),
    dets:close(DB).

printDets(_,'$end_of_table') -> ok;
printDets(DB,Key) ->
    A = dets:lookup(DB,Key),
    print(A),
    printDets(DB, dets:next(DB,Key)).

print([]) ->ok;
print([A|R])->io:format("DB: ~p~n",[A]),
            print(R).

openPrint(Filename)->
    {ok,DB} = dets:open_file(det,[{file, ?TEMP_DETS_FILE_DIR ++ Filename}]),
    printDets(DB, dets:first(DB)),
    dets:close(DB).

copy(A)->
    file:rename(?TEMP_DETS_FILE_DIR ++ ?TEMP_DETS_FILE ++ ".db",?TEMP_DETS_FILE_DIR ++ A).

tes()->io:format("AAAA~n"),
A = btests:tes(),
io:format("CCCC - ~p~n",[A]).
