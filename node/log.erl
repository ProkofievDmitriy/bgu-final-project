-module(log).
-behaviour(gen_server).

-export([info/2, debug/2, preciseDebug/2, err/2, warn/2, info/1, debug/1,
         preciseDebug/1, err/1, warn/1, dev/2, dev/1,
         critical/1, critical/2, start/0]).

 -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-include("./include/properties.hrl").
-include("./include/vcb.hrl").

-record(context, {log_file, error_log_file}).

start()->
    {ok, PID} = gen_server:start({local, ?MODULE }, ?MODULE, [], []),
    PID.

init(_Props) ->
    {ok, File} = file:open(filename:absname("file.log"), write),
    {ok, ErrorsFile} = file:open(filename:absname("errors_file.log"), write),
    io:format("[~p]: Opened file: ~p~n", [?MODULE, File]),
    {ok, #context{log_file = File, error_log_file = ErrorsFile}}.



handle_info(Request, Context)  ->
    ?LOGGER:info("[~p]: STUB Handle INFO Request(~w), Context: ~w~n", [?MODULE, Request, Context]),
	{noreply, Context}.


handle_cast({log, Level, Message, Params}, Context) ->
    writeToFile(Context#context.log_file, Message, Params),
    writeToConsole(Message, Params),
    if (Level >= 3)->
        writeToFile(Context#context.error_log_file, Message, Params);
    true ->
        ok
    end,
    {noreply, Context};

handle_cast(Request, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CAST Request(~w), Context: ~w ~n", [?MODULE, Request, Context]),
    {noreply, Context}.

handle_call(Request, From, Context) ->
    ?LOGGER:debug("[~p]: STUB Handle CALL Request(~w) from ~p, Context: ~w~n", [?MODULE, Request, From, Context]),
    {reply, ok, Context}.

terminate(Reason, Context) ->
    %TODO Proper termination of module with all consequences
    ?LOGGER:info("[~p]: STUB terminating, Reason ~p, State ~p.~n", [?MODULE, Reason, Context]),
    ok.

code_change(_OldVsn, Context, _Extra) -> {ok, Context}.



print(Level, LevelMessage, Message, Params) ->
    IsValidModule = isValidModule(Params),
    case {IsValidModule, Level} of
        {true, _} when Level >= ?CURRENT_LOG_LEVEL->
            sendLog(Level, LevelMessage, Message, Params);
        {false, _ } when Level >= 3  ->
            sendLog(Level, LevelMessage, Message, Params);
        _ ->
         ok
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

dev(Message, Params) ->
    print(-2, "[TEMP]  ", Message, Params).
dev(Message) ->
    debug(Message, []).

critical(Message, Params) ->
    print(4, "[CRITICAL] ", Message, Params).
critical(Message) ->
    debug(Message, []).

err(Message, Params) ->
    print(3, "[ERROR]  ", Message, Params).
err(Message) ->
    error(Message, []).

warn(Message, Params) ->
    print(2, "[WARN]   ", Message, Params).
warn(Message) ->
    warn(Message, []).





isValidModule([])-> true;
isValidModule(Params)->
    not lists:member(lists:nth(1, Params), ?MODULES_TO_FILTER).

writeToConsole(Message, Params)->
    case ?LOGGER_MODE of
        file -> ok;
            % io:format(LogFile, Message, Params);
        dual ->
            io:format(Message, Params);
        _ ->
            io:format(Message, Params)
    end.

writeToFile(LogFile, Message, Params)->
    case ?LOGGER_MODE of
        file ->
            io:format(LogFile, Message, Params);
        dual ->
            io:format(LogFile, Message, Params);
        _ -> ok
    end.

sendLog(Level, LevelMessage, Message, Params)->
    {Hours, Minutes, Seconds} = erlang:time(),
    Millis = utils:get_current_millis() rem 1000,
    gen_server:cast(?MODULE, {log, Level, "~p:~p:~p:~p " ++ LevelMessage ++ Message, [Hours, Minutes, Seconds, Millis] ++ Params}).
