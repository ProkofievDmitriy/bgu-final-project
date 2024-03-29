-module(ntp).
-export([ask/1, ask/0]).

-define(NTP_PORT,       123).			% udp
-define(SERVER_TIMEOUT, 1500).			% ms
-define(EPOCH,	  	2208988800).		% offset yr 1900 to unix epoch

ask() -> ask("0.asia.pool.ntp.org").

ask(Host) ->
    Got = udp_transact(Host, make_ntp_packet()),
    unpack_ntp_packet(Got) .

udp_transact(Host, Binary) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, Host, ?NTP_PORT, Binary),
    Value = receive {udp, Socket, _IP, _Port, Bin} -> Bin
            after   ?SERVER_TIMEOUT -> 0
            end,
    gen_udp:close(Socket),
    Value.

binfrac(Bin) -> binfrac(Bin, 2, 0).
binfrac(0, _, Frac) -> Frac;
binfrac(Bin, N, Frac) -> binfrac(Bin bsr 1, N*2, Frac + (Bin band 1)/N).

unpack_ntp_packet( <<
   LI:2,					% LI
   Version:3,					% Version, VN
   Mode:3,					% Mode
   Stratum:8,					% Stratum
   Poll:8/signed,				% Poll
   Precision:8/signed,				% Precision
   RootDel:32,					% root del
   RootDisp:32,					% root disp
   R1:8, R2:8, R3:8, R4:8,			% ref id
   RtsI:32, RtsF:32,				% ref ts
   OtsI:32, OtsF:32,				% originate ts
   RcvI:32, RcvF:32,				% rcv t
   XmtI:32, XmtF:32				% xmt ts
   >>) ->
   {Mega, Sec, Micro} = os:timestamp(),
    NowTimestamp = (Mega*1000000 + Sec)*1000 + round(Micro/1000),
    TransmitTimestamp = (XmtI - ?EPOCH)*1000 + round(binfrac(XmtF)*1000),
    Packet = { {li, LI}, {vn, Version}, {mode, Mode}, {stratum, Stratum},
   	{poll, Poll}, {precision, Precision}, {rootDelay, RootDel},
   	{rootDispersion, RootDisp}, {referenceId, R1, R2, R3, R4},
   	{referenceTimestamp, (RtsI - ?EPOCH)*1000 + round(binfrac(RtsF)*1000)},
   	{originateTimestamp, (OtsI - ?EPOCH)*1000 + round(binfrac(OtsF)*1000)},
   	{receiveTimestamp,   (RcvI - ?EPOCH)*1000 + round(binfrac(RcvF)*1000)},
   	{transmitTimestamp,  TransmitTimestamp},
           {clientReceiveTimestamp, NowTimestamp},
           {offset, TransmitTimestamp - NowTimestamp}
        },
    io:format("~p~n",[Packet]),
    TransmitTimestamp - NowTimestamp;

unpack_ntp_packet(_) ->
    0.


make_ntp_packet() ->
    %  LI,  VN,  Mode, rest... 384 bits overall
    << 0:2, 3:3, 3:3,  0:(3*8 + 3*32 + 4*64) >>.
