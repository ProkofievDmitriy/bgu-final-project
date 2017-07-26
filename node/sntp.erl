%%%------------------------------------------------------------------------
%%% @doc Implements SNTP query logic.
%%%      SNTP - Simple Network Time Protocol (RFC-2030).
%%%------------------------------------------------------------------------

-module(sntp).

%% External API
-export([time/1, time_servers/0, time_servers/1, avg_time/0, avg_time/1, get_offset/0]).

-include_lib("kernel/include/inet.hrl").

%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

-record(sntp, {
    version,    % NTP version (3 or 4)
    stratum,    % 1 - primary ref, 2-15 secondary ref.
    precision,  % Precision of local clock in us.
    rootdelay,  % float() - Total roundtrip delay to the primary reference source in ms.
    rootdisp,   % float() - Nominal error relative to the primary reference source in ms.
    refid,      % Server reference string
    reftime,    % Time at which the local clock was last set (in now() format).
    transtime,  % Time at which the response left NTP server (in now() format).
    delay,      % Roundtrip delay in us.
    offset      % Local clock offset in us.
}).


%%-------------------------------------------------------------------------
%% @spec () -> [ ip_address() ]
%% @doc Return a list of default NTP time servers for this host.
%% @end
%%-------------------------------------------------------------------------

get_offset()->
    Result = (catch avg_time()),
    case Result of
        {_Min, _Max, 0} ->
            io:format("AVERAGE OFFSET = 0~n"),
            0;
        {_Min, _Max, Avg}->
            io:format("AVERAGE OFFSET = ~p microseconds~n", [Avg]),
            round(Avg / 1000);
        Else ->
            io:format("UNEXPECTED RESULT = ~p~n", [Else]),
            0
    end.

time_servers() ->
    time_servers(true).

%%-------------------------------------------------------------------------
%% @spec (boolean()) -> [ ip_address() ]
%% @doc Return a list of default NTP time servers for this host. If
%%      `Resolve' is true, the list will contain IP addresses or else
%%      host names.
%% @end
%%-------------------------------------------------------------------------
time_servers(Resolve) when is_boolean(Resolve) ->
    [
     "0.asia.pool.ntp.org",
     "2.asia.pool.ntp.org",
     "1.asia.pool.ntp.org",
     "3.asia.pool.ntp.org",
     "il.pool.ntp.org",
     "pool.ntp.org"
     ].

%%-------------------------------------------------------------------------
%% @spec () -> {Min::integer(), Max::integer(), Avg::integer()}
%% @doc Query NTP time sources from `"/etc/ntp.conf"' and return
%%      min/max/avg offset of current host from given time sources.
%% @see avg_time/1
%% @end
%%-------------------------------------------------------------------------
avg_time() ->
    avg_time(time_servers()).

%%-------------------------------------------------------------------------
%% @spec (ServerAddresses) -> {Min::integer(), Max::integer(), Avg::integer()}
%%          ServerAddresses = [ ip_address() ]
%% @doc Query `ServerAddress' NTP time sources and return min/max/avg offset
%%      of current host from given time sources.
%% @end
%%-------------------------------------------------------------------------
avg_time(ServerAddresses) ->
    Results = [time(3, Addr, []) || Addr <- ServerAddresses],
    {Min, Max, Sum, N} =
        lists:foldl(
            fun(#sntp{offset=Offset}, {Min, Max, Sum, N}) ->
                {erlang:min(Min, Offset),
                 erlang:max(Max, Offset), Sum+Offset, N+1};
            (_, Acc) ->
                Acc
            end,
            {99999999, -99999999, 0, 0},
            Results),
    {Min, Max, round(Sum/N)}.

%%-------------------------------------------------------------------------
%% @spec (ServerAddress::ip_address()) -> #sntp{}
%% @doc Query `ServerAddress' time source to find out server time and
%%      current host's offset from time source.
%% @end
%%-------------------------------------------------------------------------
time(ServerAddress) ->
    {ok, S} = gen_udp:open(0, [binary, {active, false}]),
    try
        ok = gen_udp:send(S, ServerAddress, _Port = 123, encode()),
        case gen_udp:recv(S, 0, 3000) of
        {ok, {_Addr, _Port, Reply}} ->
            decode(Reply);
        Other ->
            Other
        end
    after
        gen_udp:close(S)
    end.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

%% Try several consequitive time lookup and choose the best response.
time(0, _, List) ->
    lists:foldl(
        fun(#sntp{offset=Offset} = T, #sntp{offset=Min}) when Offset < Min ->
            T;
        (_, Min) ->
            Min
        end,
        #sntp{},    %% (integer() < undefined) for any value.
        List);
time(N, Server, Acc) ->
    Res = time(Server),
    time(N-1, Server, [Res | Acc]).

encode() ->
    {Secs, US} = now_to_sntp_time(os:timestamp()),
    <<(_LI = 0):2, (_VN = 4):3, (_Mode = 3):3,
      0:8, 0:8, 0:8, 0:32, 0:32, 0:32, 0:64, 0:64, 0:64,
      Secs:32/big-integer, US:32/big-integer>>.

decode(<<LI:2, Vsn:3, Mode:3, Stratum:8, _Poll:8/signed-integer,
         Precision:8/signed-integer,    RootDelay:32/big-signed-integer,
         RootDispersion:32/big-integer, RefId:4/binary,
         RefTimeSec:32/big-integer,     RefTimeUSec:32/big-integer,
         OrigTimeSec:32/big-integer,    OrigTimeUSec:32/big-integer,
         RecvTimeSec:32/big-integer,    RecvTimeUSec:32/big-integer,
         TransTimeSec:32/big-integer,   TransTimeUSec:32/big-integer,
         _Rest/binary>>) when LI < 3, Mode >= 3 ->
    DestTime  = os:timestamp(),
    OrigTime  = sntp_time_to_now(OrigTimeSec, OrigTimeUSec),
    RecvTime  = sntp_time_to_now(RecvTimeSec,  RecvTimeUSec),
    TransTime = sntp_time_to_now(TransTimeSec, TransTimeUSec),
    Delay     = (timer:now_diff(DestTime, OrigTime) - timer:now_diff(RecvTime, TransTime)),
    Offset    = (timer:now_diff(RecvTime, OrigTime) + timer:now_diff(TransTime, DestTime)) div 2,
    [I1 | Tail] = binary_to_list(RefId),
    Ref = lists:flatten(integer_to_list(I1) ++ [[$., integer_to_list(I)] || I <- Tail]),
    #sntp{version=Vsn, stratum=Stratum, precision=round((1 / (1 bsl abs(Precision)))*1000000),
          rootdelay=RootDelay / 65.536, rootdisp=(RootDispersion * 1000) / 65536,
          refid=Ref, reftime=sntp_time_to_now(RefTimeSec,RefTimeUSec),
          transtime=TransTime, delay=Delay, offset=Offset};
decode(<<3:2, _:6, _/binary>>) ->
    {error, clock_not_synchronized};
decode(Packet) ->
    {error, {unknown_packet_format, Packet}}.

sntp_time_to_now(Sec, USec) ->
    case Sec band 16#80000000 of
    0 -> Time = Sec + 2085978496; % use base: 7-Feb-2036 @ 06:28:16 UTC
    _ -> Time = Sec - 2208988800  % use base: 1-Jan-1900 @ 01:00:00 UTC
    end,
    {Time div 1000000, Time rem 1000000, round((USec * 1000000) / (1 bsl 32))}.

now_to_sntp_time({_,_,USec} = Now) ->
    SecsSinceJan1900 = 16#80000000 bor
        (calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(Now)) - 59958230400),
    {SecsSinceJan1900, round(USec * (1 bsl 32) / 1000000)}.

%% @spec (Resolve, Name) -> ip_address() | nxdomain
resolve(_, {_, _, _, _} = IP) ->
    IP;
resolve(false, Name) ->
    Name;
resolve(true, Name) when is_list(Name) ->
    % Do a DNS lookup on the hostname
    case inet:gethostbyname(Name) of
    {ok, #hostent{h_addr_list = [Addr | _]}} -> Addr;
    _ -> nxdomain
    end.
