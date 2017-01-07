-module(log).

-export([info/2, debug/2, preciseDebug/2, err/2, warn/2, info/1, debug/1, preciseDebug/1, err/1, warn/1]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


print(Level, LevelMessage, Message, Params) ->
    IsValidModule = isValidModule(Params),
    if
        (Level >= ?CURRENT_LOG_LEVEL) and IsValidModule ->
            {Hours, Minutes, Seconds} = erlang:time(),
            io:format("~p:~p:~p ", [Hours, Minutes, Seconds]),
            io:format(LevelMessage ++ Message, Params);
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
    lists:member(lists:nth(1, Params), ?MODULES_TO_LOG).
