-module(log).

-export([info/2, debug/2, error/2, warn/2, info/1, debug/1, error/1, warn/1]).

-include("./include/properties.hrl").

print(Level, LevelMessage, Message, Params) ->
    if
        (Level >= ?CURRENT_LOG_LEVEL) -> io:format(LevelMessage ++ Message, Params);
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

error(Message, Params) ->
    print(3, "[ERROR]  ", Message, Params),
error(Message) ->
    error(Message, []).

warn(Message, Params) ->
    print(2, "[WARN]   ", Message, Params).
warn(Message) ->
    warn(Message, []).


