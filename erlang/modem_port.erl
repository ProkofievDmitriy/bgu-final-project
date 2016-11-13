-module(modem_port).
-export([start/0, stop/0, init/1]).
-export([send/2, send/1, check/0, loop_send/3, extract_crc/1, preper_msg/1, exemine_data/1]).

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

-define(BACKOFF_TIME, 250 ). 	%max backoff time

%C program detailes defines
-define(PROGRAM_PATH, "/ISG/node/").
-define(PROGRAM_NAME, "port").

%Processes registered Name defines
-define(SUPERVISOR, supervisor).
-define(MODEM_PORT, modem_port).

%%%%%%%%%%%%%%%%%%%%%%%Channel%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		API			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%start the supervised modem port module.
start() ->
		Caller_PID = self(),
    P = spawn(fun()-> init_supervisor(Caller_PID,0) end),
		register(?SUPERVISOR, P), P.

%stop the running of the port, which eill end the supervisor
stop() ->
    ?SUPERVISOR ! stop.



%send Packet via channel Channel, with payload Payload.
% Channel : 1 - RF , 2 - PLC , 3 RF + PLC 
send(Channel, Payload) ->
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


loop_send( _Channel , N, _S) when N =:= 0 -> done;
loop_send(Channel, N, S) -> 
		receive after 500*S-> io:format("mode_port:sending.....~n"),
		X = random:uniform(2),
		if X == 1 -> Y = 16; true-> Y=36 end,
		
		Payload = [N rem 255] ++ lists:seq(15, 15+Y),
		io:format("loop_send: size of payload is:~p~n", [length(Payload)]),
		send(Channel), 
		loop_send(Channel, N-1, S) end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		Supervision			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%wait for massages or termination of supervised process (modem_port).
supervise() ->
	receive
		stop -> ?MODEM_PORT ! {stop, normal}, exit(normal);
		%modem port terminated normaly, allow it and exit supervisor with normal reason
		 {'DOWN', _Ref, process, _Pid, normal} -> io:format("mode_port:modem_port terminated normally~n"), 1;%exit(normal);
		%modem port terminated for unknown reason, return 1 for incrementing port's crashes 
		 {'DOWN', _Ref, process, _Pid, port_terminated} -> io:format("mode_port:modem_port terminated because of some error11~n"), 1;
		%modem port terminated due to too many errors (unsent packets or damaged packets), return 1 for incrementing port's crashes 
		 {'DOWN', _Ref, process, _Pid, port_errors} -> io:format("mode_port:modem_port terminated because of port errors22~n"), 1;
	   	{'DOWN', _Ref, process, _Pid, Else} ->io:format("mode_port:modem_port terminated unknown reason: ~p.~n", [Else]), 1
	end.

%in case too many port crashes occured, terminate
init_supervisor( _Caller_PID, Port_crash) when Port_crash =:= ?PORTS_CRASH_LIMIT -> 
	io:format("modem_port:init_supervise: too many port crashes!!!! wow!!!! something is really not working~n"),
	exit(modem_not_available); %%optionally, tell Caller (using caller PID) that module is'nt functioning
%spawn/respawn modem_port. create monitor ref. monitor the modem_port's terminations. if termination not normal, call itself with Port_crash counter incremented.
init_supervisor( Caller_PID, Port_crash) -> 
	PRG = ?PROGRAM_PATH ++ ?PROGRAM_NAME,
	Still_opened_port_processes_list = os:cmd("pidof " ++ PRG ),
	close_all_port_processes(Still_opened_port_processes_list, []),
	PID = spawn(?MODULE, init, [ Caller_PID]),	%init function of modem_port core
	Ref = erlang:monitor(process, PID),
	Error = supervise(),
	erlang:demonitor(Ref),
	init_supervisor( Caller_PID, Port_crash + Error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		PROGRAM			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_port(Msg) ->
  ?MODEM_PORT ! {call , Msg}.
	
%initiate modem_port. register it's name, compile c port's program, config pins of controller, open port to c program, and call for loop function
init(Super_PID) ->
	P = self(),
    register(?MODEM_PORT, P),
    compile_c_port(),
    os:cmd("sh uart_config.sh"),
    process_flag(trap_exit, true),
    PRG = ?PROGRAM_PATH ++ ?PROGRAM_NAME,
    Port = open_port({spawn, PRG}, [{packet, 2}]),
    OS_PID = extract_os_pid(Port),
    X = os:timestamp(),
	random:seed(X),
    io:format("modem_port: done init~n"),
    loop(Port, OS_PID, Super_PID,0).
    
%if number of errors between erlang port to c port passed limit, terminate c program and modem_port (with port_errors reason).
loop(_, OS_PID ,_,Port_Errors) when Port_Errors =:= ?PORTS_ERRORS_LIMIT -> 
	io:format("mode_port:too many port errors. terminate myself!!!!~n"),
	close_c_port_program(OS_PID),
	self() ! {stop,port_errors};




loop(Port, OS_PID, Super_PID, Port_Errors) ->
    receive
	%Caller wants to send Packet to c port
	{call, Msg} ->
			MSG = preper_msg(Msg),
			if is_list(MSG) -> sendMsg(Port, MSG); true -> io:format("error prepering msg ~p~n", [MSG]),dont_send end, 
			%Error = wait_for_ack(),		%return 0 if ack received, 1 if nack
			%loop(Port, OS_PID, Super_PID, Port_Errors + Error);
			loop(Port, OS_PID, Super_PID, Port_Errors );
			
	%c port sent data
	{_Port2, {data, Data}} ->		
			Ans = exemine_data2(Data),		%return 0 if data is of correct format, 1 otherwise
			case Ans of
				% first byte not recognized
				channel_error->	io:format("modem_port: got msg: ~p~n but didnt pass exemine reason:~p~n",[Data,Ans]);
				crc_error -> io:format("*************~nmodem_port: got msg: ~p~n but didnt pass exemine reason:~p~n***************~n",[Data,Ans]);
				crc_error_try_split -> io:format("*************~nmodem_port: got msg: ~p~n but didnt pass exemine reason:~p. now split data and check splits~n***************~n",[Data,Ans]);
				_List -> hyRPL:send_message_Fsm(Ans) %(channel , rssi, payload)
			end,
			loop(Port, OS_PID, Super_PID, Port_Errors );
			
	%Something want to stop erlang's port run
	{stop, Reason} ->
			P = self(),
	    Port ! {P, close},
	    close_c_port_program(OS_PID),
	    receive
		{Port, closed} -> io:format("%%%%%%%~n~n"), exit(Reason)
	    end;
	%c port terminates unexpectedly
	{'EXIT', _Port, Reason} ->
		io:format("mode_port:got exit from c port. reason is:~p.~n", [Reason]),
	   close_c_port_program(OS_PID), exit(port_terminated) 
    end.






extract_crc(List) ->
	extract_crc(List, [],  lists:flatlength(List)).
extract_crc( LCRC, Rest,X) when X =:= 4 -> {LCRC, Rest};
extract_crc( [H|T] , Rest, X) -> extract_crc(T, Rest ++ [H] , X-1). 
	
%check received data. returns 1 if data !not! at the rigth packet format. returns 0 if it is the rigth format.
exemine_data( [Channel | _ ]) when (Channel < 0) orelse (Channel > 2) -> channel_error;  
exemine_data( [Channel, RSSI | Rest]) -> 
	{LCRC, L_NO_CRC} = extract_crc(Rest),
	CRC = binary:decode_unsigned(list_to_binary(LCRC)),
	%io:format("CRC is:~p~n", [CRC]),
	CRC2 = erlang:crc32(L_NO_CRC),
	%io:format("CRC2 is:~p. L_NO_CRC is:~p~n", [CRC2, L_NO_CRC]),
	case CRC2  of
		CRC -> [Channel] ++ [RSSI] ++ L_NO_CRC;
		_Else ->   crc_error
	end.

exemine_data2(L) -> X = length(L), 
	Ans = exemine_data(L),
	case Ans of
		channel_error -> channel_error;
		crc_error->
			if X>20 ->
			L1 = lists:sublist(L, 1, 20),
			L2 = lists:sublist(L,21,X),
			Self = self(),
			Self!{port,{data, L1}}, Self!{port,{data,L2}},
			crc_error_try_split;
			true -> crc_error
			end;
		_List -> Ans
	end.
			
			





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		C PORT FUNCTIONS			%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_c_port() ->
		S = "gcc "++ ?PROGRAM_PATH ++ ?PROGRAM_NAME ++ ".c" ++ " -o " ++ ?PROGRAM_NAME,
		_STR = os:cmd(S),
		_STR2 = os:cmd("chmod 777 " ++ ?PROGRAM_PATH ++ ?PROGRAM_NAME).

extract_os_pid(Port) ->  
    L = erlang:port_info(Port),
    F = fun({ATOM, _VAL}) -> ATOM=:=os_pid end,
    [{os_pid, OS_PID}] = lists:filter(F,L),
    OS_PID.
    
close_c_port_program(OS_PID)->
		os:cmd("kill "++integer_to_list(OS_PID)),
		io:format("closed c program for real~n").



%%this function  create for each sent byte a header (size of byte) according to erlang - c port protocol :
%(?START_BYTE for the first byte of packet, ?END_BYTE for last byte of packet, ?REG_BYTE for rest).
%after attaching the header it sends it to port using sendByte/3
sendMsg(Port, MSG) ->
	[H|T] = MSG,
	backoff(H),
	io:format("\tMSG is:~p~n~n", [MSG]),
	
	sendByte(Port, H, ?START_BYTE),
	sendMSG2(Port, T).
sendMSG2(Port, [H|T]) when T =:= [] -> 
	sendByte(Port, H, ?END_BYTE);
sendMSG2(Port, [H|T]) -> 
	sendByte(Port, H, ?REG_BYTE),
	sendMSG2(Port, T).

%this function get a port to send data to, a byte of info and it's header. it send the minipacket ([Header, Date_byte]) to the port.
sendByte(Port, Byte, Type) -> P=self(), 
	Port ! {P, {command, [Type,Byte]}}, sent.


close_all_port_processes([],_) -> done;
close_all_port_processes([H|T] , L2) ->
	case H of 
		10 -> os:cmd("kill " ++ L2);
		32 -> os:cmd("kill " ++ L2), 
				close_all_port_processes(T,[]);
		Else -> close_all_port_processes(T, L2 ++ [Else])
	end.

crc_to_list(N) ->
	binary:bin_to_list(binary:encode_unsigned(N)).

preper_msg([Channel, _Reserved | _Rest]) when (Channel > 3) orelse (Channel<0) -> io:format("bad channel~p~n",[Channel]),ignore_msg;
preper_msg([Channel, _ | Rest]) -> Size = lists:flatlength(Rest), 
	case Size of
		S1 when S1 =< 14 -> X = 20 - Size - 4 - 2, if X > 0 -> PAD = lists:seq(1,X); true -> PAD = [] end ,
						    L = Rest ++ PAD, CRC = erlang:crc32(L), LCRC = crc_to_list(CRC), 
						    %io:format("LCRC is:~p~n,", [LCRC]),
							[Channel] ++ [0] ++ L ++ LCRC;
		S2 when S2 =< 34 -> X = 40 - Size - 4 - 2, if X > 0 -> PAD = lists:seq(1,X); true -> PAD = [] end ,
						    L = Rest ++ PAD, CRC = erlang:crc32(L), LCRC = crc_to_list(CRC), 
							[Channel] ++ [0] ++ L ++ LCRC;
		S2 when S2 =< 60 -> X = 20 - Size - 4 - 2, if X > 0 -> PAD = lists:seq(1,X); true -> PAD = [] end ,
						    L = Rest ++ PAD, CRC = erlang:crc32(L), LCRC = crc_to_list(CRC), 
							[Channel] ++ [0] ++ L ++ LCRC;
		_ -> too_large
	end.


backoff(Channel)  when Channel =:=1 ->
	Wait = random:uniform(?BACKOFF_TIME),
	%io:format("~nbackoff - (~p)", [Wait]),
	receive after Wait -> ready end;
backoff(Channel) -> plc_has_backoff.
