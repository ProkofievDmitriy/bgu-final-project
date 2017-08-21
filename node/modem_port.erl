-module(modem_port).
-export([start/1, stop/0, init/2]).
-export([send/2, send/1, check/0, extract_crc/1, prepare_payload/1, examine_data/1]).
-include("./include/properties.hrl").
-include("./include/vcb.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%	defines	%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%General defines
-define(OK, 1).
-define(ERROR, -1).

%Erlang <--> c port defines
-define(START_BYTE, 0).
-define(END_BYTE, 255).
-define(REG_BYTE, 1).

%Limits defines
-define(PORTS_ERRORS_LIMIT, 10).
-define(PORTS_CRASH_LIMIT, 2).

-define(BACKOFF_TIME, 500 ). 	%max backoff time

%C program detailes defines
-define(PROGRAM_PATH, "./c-port/").
-define(PROGRAM_NAME, "port").

%Processes registered Name defines
-define(SUPERVISOR, supervisor).

%%%%%%%%%%%%%%%%%%%%%%%Channel%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		API			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%start the supervised modem port module.
start(DataLinkFsmPid) ->
        ?LOGGER:debug("[~p]: start, DataLinkFsmPid: ~p~n", [?MODULE, DataLinkFsmPid]),
		Caller_PID = self(),
        P = spawn(fun()-> init_supervisor(Caller_PID, 0, DataLinkFsmPid) end),
		register(?SUPERVISOR, P),
		P.

%stop the running of the port, which will end the supervisor

stop()->
    try
        stop_internal()
    of
        Result -> Result
    catch
        _:Reason -> ?LOGGER:err("[~p]: stop failed : ~w~n",[?MODULE, Reason]),
        Reason
    end.


stop_internal() ->
	?LOGGER:debug("[~p]: stop~n", [?MODULE]),
    ?SUPERVISOR ! stop.



%send Packet via channel Channel, with payload Payload.
% Channel : 1 - RF , 2 - PLC , 3 RF + PLC
send(Channel, Payload) ->
	?LOGGER:debug("[~p]: send: channel: ~w, payload ~w ~n", [?MODULE, Channel, Payload]),
		MSG = [Channel] ++ [0] ++ Payload,
    call_port(MSG).
%main use. user wants to sent a packet (list of bytes - integer of range [0,255])
send(Packet) when is_list(Packet) ->
	call_port(Packet);
%user wants to sent some packet via channel Channel
send(Channel) ->
		send(Channel, lists:seq(49,49+25)).


%check if modem port is running
check()-> ?MODEM_PORT ! check.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		Supervision			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%wait for massages or termination of supervised process (modem_port).
supervise() ->
	receive
		stop -> ?MODEM_PORT ! {stop, normal}, exit(normal);
		%modem port terminated normaly, allow it and exit supervisor with normal reason
		 {'DOWN', _Ref, process, _Pid, normal} -> ?LOGGER:err("[~p]: terminated normally~n", [?MODULE]), 1;%exit(normal);
		%modem port terminated for unknown reason, return 1 for incrementing port's crashes
		 {'DOWN', _Ref, process, _Pid, port_terminated} -> ?LOGGER:err("[~p]: terminated because of some error11~n", [?MODULE]), 1;
		%modem port terminated due to too many errors (unsent packets or damaged packets), return 1 for incrementing port's crashes
		 {'DOWN', _Ref, process, _Pid, port_errors} -> ?LOGGER:err("[~p]: terminated because of port errors22~n", [?MODULE]), 1;
	   	{'DOWN', _Ref, process, _Pid, Else} ->?LOGGER:err("[~p]: terminated unknown reason: ~p.~n", [?MODULE, Else]), 1
	end.

%in case too many port crashes occured, terminate
init_supervisor( _Caller_PID, Port_crash, _DataLinkFsmPid) when Port_crash =:= ?PORTS_CRASH_LIMIT ->
	?LOGGER:err("[~p]: modem_port:init_supervise: too many port crashes!!!! wow!!!! something is really not working~n", [?MODULE]),
	exit(modem_not_available); %%optionally, tell Caller (using caller PID) that module is'nt functioning
%spawn/respawn modem_port. create monitor ref. monitor the modem_port's terminations. if termination not normal, call itself with Port_crash counter incremented.
init_supervisor( Caller_PID, Port_crash, DataLinkFsmPid) ->
	?LOGGER:debug("[~p]: init supervisor~n", [?MODULE]),
%	PRG = ?PROGRAM_PATH ++ ?PROGRAM_NAME,
	PRG = ?PROGRAM_NAME,
	Still_opened_port_processes_list = os:cmd("pidof " ++ PRG ),
	close_all_port_processes(Still_opened_port_processes_list, []),
	PID = spawn(?MODULE, init, [ Caller_PID, DataLinkFsmPid]),	%init function of modem_port core
	Ref = erlang:monitor(process, PID),
	Error = supervise(),
	erlang:demonitor(Ref),
	init_supervisor( Caller_PID, Port_crash + Error, DataLinkFsmPid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		PROGRAM			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_port(Message)->
    try
        call_port_internal(Message)
     of
        Result -> Result
    catch
        _:Reason ->
            ?LOGGER:err("[~p]: call port failed : ~p~n",[?MODULE, Reason]),
            Reason
    end.

call_port_internal(Msg) ->
  ?MODEM_PORT ! {call , Msg},
  receive
    Result ->
        Result
  after ?TIMEOUT ->
        {error, timeout_in_modem_port}
  end.

%initiate modem_port. register it's name, compile c port's program, config pins of controller, open port to c program, and call for loop function
init(Super_PID, DataLinkFsmPid) ->
	P = self(),
    ?LOGGER:debug("[~p]: start init ~p~n", [?MODULE, P]),
    register(?MODEM_PORT, P),
    compile_c_port(),
    UartConfigResult = os:cmd("./scripts/uart_config.sh"),
    case UartConfigResult of
        [] ->   ?LOGGER:debug("[~p]: UART CONFIGURED SUCCESSFULLY~n", [?MODULE]);
        _ ->
            ?LOGGER:critical("[~p]: UART FAILED TO CONFIG : Result: ~p~n~n", [?MODULE, UartConfigResult])
    end,
    process_flag(trap_exit, true),
    PRG = "./" ++ ?PROGRAM_NAME,
    ?LOGGER:debug("[~p]: start port:  ~p~n", [?MODULE, PRG]),
    Port = open_port({spawn, PRG}, [{packet, 2}]),
    OS_PID = extract_os_pid(Port),
    X = os:timestamp(),
	random:seed(X),
    ?LOGGER:debug("[~p]: done init~n", [?MODULE]),
    loop(Port, OS_PID, Super_PID, 0, DataLinkFsmPid).

%if number of errors between erlang port to c port passed limit, terminate c program and modem_port (with port_errors reason).
loop(_, OS_PID ,_,Port_Errors, _DataLinkFsmPid) when Port_Errors =:= ?PORTS_ERRORS_LIMIT ->
	?LOGGER:debug("[~p]: too many port errors. terminate myself!!!!~n", [?MODULE]),
	close_c_port_program(OS_PID),
	self() ! {stop,port_errors};




loop(Port, OS_PID, Super_PID, Port_Errors, DataLinkFsmPid) ->
    receive
	%Caller wants to send Packet to c port
	{call, Msg} ->
            ?LOGGER:preciseDebug("[~p]: Received CALL MSG : ~w~n", [?MODULE, Msg]),
			MSG = prepare_payload(Msg), % MSG = <<Channel:8, Rest/bitstring>>
			if is_list(MSG) ->
			    DataLinkFsmPid ! sendMsg(Port, MSG);
			    true -> ?LOGGER:err("[~p]: error on message prepare : ~w~n", [?MODULE, MSG])
			end,
			%Error = wait_for_ack(),		%return 0 if ack received, 1 if nack
			%loop(Port, OS_PID, Super_PID, Port_Errors + Error);
			loop(Port, OS_PID, Super_PID, Port_Errors, DataLinkFsmPid);

	%c port sent data
	{_Port2, {data, Data}} ->
			?LOGGER:debug("[~p]: Received data from port: length ~p,  Data ~w~n", [?MODULE, length(Data), Data]),
			Ans = examine_data2(Data),		%return 0 if data is of correct format, 1 otherwise
			case Ans of
				% first byte not recognized
				channel_error->	?LOGGER:preciseDebug("[~p]: got msg: ~p~n but didnt pass exemine reason:~p~n",[?MODULE, Data,Ans]);
				crc_error -> ?LOGGER:preciseDebug("[~p]: got msg: ~p but didnt pass exemine reason:~p~n",[?MODULE, Data,Ans]);
				crc_error_try_split -> ?LOGGER:preciseDebug("[~p]: got msg: ~p but didnt pass exemine reason:~p. now split data and check splits~n",[?MODULE, Data,Ans]);
				_List -> ?DATA_LINK:handle_incoming_message(DataLinkFsmPid, Ans) %(channel , rssi, payload)
			end,
			loop(Port, OS_PID, Super_PID, Port_Errors, DataLinkFsmPid);

	%Something want to stop erlang's port run
	{stop, Reason} ->
        ?LOGGER:debug("[~p]: Received stop : Reason: ~p~n", [?MODULE, Reason]),
        P = self(),
	    Port ! {P, close},
	    close_c_port_program(OS_PID),
	    receive
		{Port, closed} -> ?LOGGER:debug("[~p]: %%%%%%%~n~n", [?MODULE]), exit(Reason)
	    end;
	%c port terminates unexpectedly
	{'EXIT', _Port, Reason} ->
		?LOGGER:debug("[~p]: got exit from c port. reason is:~w.~n", [?MODULE, Reason]),
	   close_c_port_program(OS_PID), exit(port_terminated)
    end.






extract_crc(List) ->
	extract_crc(List, [],  lists:flatlength(List)).
extract_crc( LCRC, Rest, 4) -> {LCRC, Rest};
extract_crc( [H|T] , Rest, X) -> extract_crc(T, Rest ++ [H] , X-1);
extract_crc( _LCRC, _Rest, _X)->  crc_error.

%check received data. returns 1 if data !not! at the rigth packet format. returns 0 if it is the rigth format.
examine_data( [Channel | _ ]) when (Channel < 0) orelse (Channel > 2) -> channel_error;
examine_data( [_Channel, _RSSI | []]) -> crc_error;
examine_data( []) -> channel_error;
examine_data( [Channel, RSSI | Rest]) ->
    case  extract_crc(Rest) of
        {LCRC, L_NO_CRC} ->
        	CRC = binary:decode_unsigned(list_to_binary(LCRC)),
        	?LOGGER:preciseDebug("[~p]: CRC is:~w~n", [?MODULE, CRC]),
        	CRC2 = erlang:crc32(L_NO_CRC),
        	?LOGGER:preciseDebug("[~p]: CRC2 is:~w. L_NO_CRC is:~w~n", [?MODULE, CRC2, L_NO_CRC]),
        	case CRC2  of
        		CRC -> [Channel] ++ [RSSI] ++ L_NO_CRC;
        		_Else ->   crc_error
    	    end;
        _ -> crc_error
    end;
examine_data(_)-> crc_error.

examine_data2(L) -> X = length(L),
	Ans = examine_data(L),
	case Ans of
		channel_error -> channel_error;
		crc_error->
			if X>20 ->
			    L1 = lists:sublist(L, 1, 20),
			    L2 = lists:sublist(L,21,X),
			    Self = self(),
			    Self!{port,{data, L1}}, Self!{port,{data,L2}},
			    crc_error_try_split;
			true ->
			    crc_error
			end;
		_List -> Ans
	end.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		C PORT FUNCTIONS			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_c_port() ->
		S = "gcc "++ ?PROGRAM_PATH ++ ?PROGRAM_NAME ++ ".c" ++ " -o " ++ ?PROGRAM_NAME,
		?LOGGER:debug("[~p]: compile C port command : ~p~n", [?MODULE, S]),
		_STR = os:cmd(S),
		_STR2 = os:cmd("chmod 777 " ++ ?PROGRAM_NAME).

extract_os_pid(Port) ->
    L = erlang:port_info(Port),
    F = fun({ATOM, _VAL}) -> ATOM=:=os_pid end,
    [{os_pid, OS_PID}] = lists:filter(F,L),
    OS_PID.

close_c_port_program(OS_PID)->
		os:cmd("kill "++integer_to_list(OS_PID)),
		?LOGGER:debug("[~p]: closed c program for real~n", [?MODULE]).



%%this function  create for each sent byte a header (size of byte) according to erlang - c port protocol :
%(?START_BYTE for the first byte of packet, ?END_BYTE for last byte of packet, ?REG_BYTE for rest).
%after attaching the header it sends it to port using sendByte/3
sendMsg(Port, MSG) ->
	[H|T] = MSG,
	backoff(H),
	?LOGGER:debug("[~p]: sendMsg ~w bytes, MSG: ~w~n", [?MODULE, length(MSG), MSG]),
	sendByte(Port, H, ?START_BYTE),
	sendMSG2(Port, T).


sendMSG2(Port, [H|[]]) ->
	sendByte(Port, H, ?END_BYTE);


sendMSG2(Port, [H|T]) ->
	sendByte(Port, H, ?REG_BYTE),
	sendMSG2(Port, T).

%this function get a port to send data to, a byte of info and it's header. it send the minipacket ([Header, Date_byte]) to the port.
sendByte(Port, Byte, Type) -> P=self(),
 	?LOGGER:preciseDebug("[~p]: sendByte: Byte: ~w~n", [?MODULE, Byte]),
	Port ! {P, {command, [Type,Byte]}},
	{ok, sent}.


close_all_port_processes([],_) -> done;
close_all_port_processes([H|T] , L2) ->
	case H of
		10 -> os:cmd("kill " ++ L2);
		32 -> os:cmd("kill " ++ L2),
				close_all_port_processes(T,[]);
		Else -> close_all_port_processes(T, L2 ++ [Else])
	end.

prepare_payload([Channel, _Reserved | _Rest]) when (Channel > 3) orelse (Channel<0) -> ?LOGGER:debug("[~p]: bad channel~p~n",[?MODULE, Channel]),ignore_msg;
prepare_payload([Channel, _ | Rest]) ->
    BitSize = bit_size(Rest),
    PaddingBitString = getPaddingBitString(BitSize),
	Result = case PaddingBitString of
	    too_large ->
            ?LOGGER:err("[~p]: MESSAGE IS TOO LARGE, SIZE: ~w.~n", [?MODULE, BitSize]),
	        {error, too_large};
        [] ->
            ?LOGGER:debug("[~p]: prepare_payload: PaddingBitString SIZE: ~w, LISTS AFTER PADDING: ~w(~w).~n", [?MODULE, 0, byte_size(Rest), bit_size(Rest)]),
            CRC = erlang:crc32(Rest),
            BinaryCRC = <<CRC:32>>,
            BinaryZero = <<0:8>>,
            ?LOGGER:preciseDebug("[~p]: CRC is:~w~n,", [?MODULE, CRC]),
            BinaryChannel = <<Channel:8>>,
            ListToSend = binary:bin_to_list(<<BinaryChannel/bitstring, BinaryZero/bitstring, Rest/bitstring ,BinaryCRC/bitstring >>),
            ?LOGGER:preciseDebug("[~p]: prepare_payload: BYTE SIZE = ~w, Result: ~w~n", [?MODULE, length(ListToSend), ListToSend]),
	        ListToSend;
        _ ->
            L = <<Rest/bitstring, PaddingBitString/bitstring>>,
            ?LOGGER:debug("[~p]: prepare_payload: PaddingBitString SIZE: ~w, LISTS AFTER PADDING: ~w(~w).~n", [?MODULE, bit_size(PaddingBitString), byte_size(L), bit_size(L)]),
            CRC = erlang:crc32(L),
            BinaryCRC = <<CRC:32>>,
            BinaryZero = <<0:8>>,
            ?LOGGER:preciseDebug("[~p]: CRC is:~w~n,", [?MODULE, CRC]),
            BinaryChannel = <<Channel:8>>,
            ListToSend = binary:bin_to_list(<<BinaryChannel/bitstring, BinaryZero/bitstring, L/bitstring ,BinaryCRC/bitstring >>),
	        ?LOGGER:preciseDebug("[~p]: prepare_payload: BYTE SIZE = ~w, Result: ~w~n", [?MODULE, length(ListToSend), ListToSend]),
	        ListToSend
    end,
	Result.


getPaddingBitString(BitSize)->
    ?LOGGER:preciseDebug("[~p]: getPaddingBitString: Size: ~w~n", [?MODULE, BitSize]),
    Result = case BitSize of
		S1 when S1 =< 112 -> %under 20 bytes
		    X = 160 - BitSize - 48,
            if X > 0 ->
                    a(X);
                true ->
                    []
            end;
		S2 when S2 =< 272 -> %under 40 bytes
		    X = 320 - BitSize - 48,
		    if X > 0 ->
		        a(X);
		        true -> []
            end;
		S2 when S2 =< 432 -> %under 60 bytes
		    X = 480 - BitSize - 48,
		    if X > 0 ->
		        a(X);
		        true -> []
            end;
		_ -> too_large
	end,
	Result.

a(Result, 0) -> Result;
a(Acc, Number) -> a(<<Acc/bitstring, 0:1>>, Number - 1).
a(Number) -> a(<<0:1>>, Number - 1).

pad_list(DestinationBinary, List) ->
	?LOGGER:debug("[~p]: pad_list: DestinationBinary: ~w, List: ~w ~n", [?MODULE, DestinationBinary, List]),
    F = fun(Num, Acc) ->
%	    ?LOGGER:debug("[~p]: fun in pad list: Acc: ~p, B: ~p ~n", [?MODULE, Acc, Num]),
	    BinNum = <<Num:8>>,
        <<Acc/bitstring, BinNum/binary>>
    end,
    lists:foldr(F, DestinationBinary, List).




backoff(Channel)  when Channel =:=1 ->
	Wait = random:uniform(?BACKOFF_TIME),
	?LOGGER:preciseDebug("[~p]: backoff - (~p)", [?MODULE, Wait]),
	receive after Wait -> ready end;
backoff(Channel) ->
    ?LOGGER:preciseDebug("[~p]: BACKOFF on channel ~p~n", [?MODULE, Channel]),
    plc_has_backoff.
