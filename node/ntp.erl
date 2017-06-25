-module(ntp).
-export([ask/1, ask/0]).

-define(NTP_PORT,       123).			% udp
-define(SERVER_TIMEOUT, 1500).			% ms
-define(EPOCH,	  	2208988800).		% offset yr 1900 to unix epoch

ask() -> ask("pool.ntp.org").

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
   _LI:2,					% LI
   _Version:3,					% Version, VN
   _Mode:3,					% Mode
   _Stratum:8,					% Stratum
   _Poll:8/signed,				% Poll
   _Precision:8/signed,				% Precision
   _RootDel:32,					% root del
   _RootDisp:32,					% root disp
   _R1:8, _R2:8, _R3:8, _R4:8,			% ref id
   _RtsI:32, _RtsF:32,				% ref ts
   _OtsI:32, _OtsF:32,				% originate ts
   _RcvI:32, _RcvF:32,				% rcv t
   XmtI:32, XmtF:32				% xmt ts
   >>)
   ->
    % {NowMS,NowS,NowUS} = now(),
    % NowTimestamp = NowMS * 1.0e6 + NowS + NowUS/1000,
    % TransmitTimestamp = XmtI - ?EPOCH + binfrac(XmtF),
    % { {li, LI}, {vn, Version}, {mode, Mode}, {stratum, Stratum},
	% {poll, Poll}, {precision, Precision}, {rootDelay, RootDel},
	% {rootDispersion, RootDisp}, {referenceId, R1, R2, R3, R4},
	% {referenceTimestamp, RtsI - ?EPOCH + binfrac(RtsF)},
	% {originateTimestamp, OtsI - ?EPOCH + binfrac(OtsF)},
	% {receiveTimestamp,   RcvI - ?EPOCH + binfrac(RcvF)},
	% {transmitTimestamp,  TransmitTimestamp},
    %     {clientReceiveTimestamp, NowTimestamp},
    %     {offset, TransmitTimestamp - NowTimestamp}
    %  },
     XmtI - ?EPOCH + binfrac(XmtF);

unpack_ntp_packet(_) ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

make_ntp_packet() ->
    %  LI,  VN,  Mode, rest... 384 bits overall
    << 0:2, 3:3, 3:3,  0:(3*8 + 3*32 + 4*64) >>.
