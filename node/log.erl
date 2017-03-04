-module(log).

-export([info/2, debug/2, preciseDebug/2, err/2, warn/2, info/1, debug/1, preciseDebug/1, err/1, warn/1, temporaryInfo/2, temporaryInfo/1]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


print(Level, LevelMessage, Message, Params) ->
    IsValidModule = isValidModule(Params),
    if
        IsValidModule ->
            if (Level >= ?CURRENT_LOG_LEVEL) ->
                {Hours, Minutes, Seconds} = erlang:time(),
                io:format("~p:~p:~p " ++ LevelMessage ++ Message, [Hours, Minutes, Seconds] ++ Params);
                true -> ok
            end;
        true -> ok
    end.


info(Message, Params) ->
    print(1, "[INFO]   ", Message, Params).
info(Message) ->
    info(Message, []).

debug(Message, Params) ->
    print(0, "[DEBUG]  ", Message, Params).
debug(Message) ->
    debug(Message, []).

preciseDebug(Message, Params) ->
    print(-1, "[DEBUG+] ", Message, Params).
preciseDebug(Message) ->
    debug(Message, []).

temporaryInfo(Message, Params) ->
    print(4, "[TEMPORARY INFO] ", Message, Params).
temporaryInfo(Message) ->
    debug(Message, []).

err(Message, Params) ->
    print(3, "[ERROR]  ", Message, Params).
err(Message) ->
    error(Message, []).

warn(Message, Params) ->
    print(2, "[WARN]   ", Message, Params).
warn(Message) ->
    warn(Message, []).


isValidModule([])-> false;
isValidModule(Params)->
    not lists:member(lists:nth(1, Params), ?MODULES_TO_FILTER).
