-module(hyRPL).
-export([outmessage/1, incoming_message/2, send_message_Fsm/1, start/1, send_data/1, send_data_stat/2, up_data_counter_origen/0, receiver_count_up/0, stop/0]).
-include("macros.hrl").

stop()->
	send_message_Fsm(stop).

incoming_message(Fsm_pid,Fsm_Name) ->
	receive
		stop -> gen_fsm:send_event(Fsm_pid, stop), 
			io:format("Incoming message is terminated ~n"),
			incoming_message(Fsm_pid,Fsm_Name);
		terminate -> bye;
		Message -> %io:format("incoming message! ~n"),
			Transmission_medium = lists:nth(1,Message),
			N_RSSI = lists:nth(2,Message),
			OP_RSSI = 255 - N_RSSI,
			if
				OP_RSSI < 255/10 -> RSSI = 0;
				OP_RSSI >= 255/10 andalso OP_RSSI < 2*255/10 -> RSSI = 1;
				OP_RSSI >= 2*255/10 andalso OP_RSSI < 3*255/10 -> RSSI = 2;
				OP_RSSI >= 3*255/10 andalso OP_RSSI < 4*255/10 -> RSSI = 3;
				OP_RSSI >= 4*255/10 andalso OP_RSSI < 5*255/10 -> RSSI = 4;
				OP_RSSI >= 5*255/10 andalso OP_RSSI < 6*255/10 -> RSSI = 5;
				OP_RSSI >= 6*255/10 andalso OP_RSSI < 7*255/10 -> RSSI = 6;
				OP_RSSI >= 7*255/10 andalso OP_RSSI < 8*255/10 -> RSSI = 7;
				OP_RSSI >= 8*255/10 andalso OP_RSSI < 9*255/10 -> RSSI = 8;
				OP_RSSI >= 9*255/10 -> RSSI = 9
			end,
			Payload = lists:nthtail(2,Message),
			Message_type = lists:nth(1,Payload),
			Payload_length = length(Payload),
			case Transmission_medium of
				1 -> BitRate = ?BitRateRF;
				2 -> BitRate = ?BitRatePLC;
				_ -> io:format("unknown transmission medium. BitRate initialized to 1 by default~n"),
					 BitRate = 1,
					 incoming_message(Fsm_pid,Fsm_Name)
			end,			
			SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
			case Message_type of
				1 -> 
					if
						Payload_length < 8 -> Index_in = -1, io:format("error, DIO message corrupted!~n");
						true ->
							%SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
							FirstByteRank = lists:nth(4,Payload),
							SecondByteRank = lists:nth(5,Payload),
							VersionNumber = lists:nth(6,Payload),
							NumberOfHops = lists:nth(7,Payload),
							Rank1 = FirstByteRank*trunc(math:pow(2,8)) + SecondByteRank,
							MaxRank = trunc(math:pow(2,16)) -1,
							case Rank1 of
								MaxRank -> Rank = infinity,
									   NumOfHops = infinity,
									   if
										NumberOfHops < 255 -> io:format("illegal incoming DIO message, rank is finite but the number of hops is infinity ~n"),
												      incoming_message(Fsm_pid,Fsm_Name);
										true -> ok
									   end;
								_ -> Rank = Rank1,
								     case NumberOfHops of
									255 -> io:format("illegal incoming DIO message, rank is finite but the number of hops is infinity ~n"),
										NumOfHops = -1,
										incoming_message(Fsm_pid,Fsm_Name);
									_ -> NumOfHops = NumberOfHops
								     end
								     
							end,
							Index_in = lists:nth(8,Payload),
							% case Transmission_medium of
							% 	1 -> BitRate = ?BitRateRF;
							% 	2 -> BitRate = ?BitRatePLC;
							% 	_ -> io:format("unknown transmission medium. BitRate initialized to 1 by default~n"),
 						% 			  BitRate = 1,
 						% 			 incoming_message(Fsm_pid,Fsm_Name)
							% end,
							%io:format("Line before the sending the message to the fsm ~p~n",[Fsm_pid]),
							try
								gen_fsm:send_event(Fsm_Name, {Message_type,[SenderNumber,ReceiverNumber,Rank,VersionNumber,NumOfHops,RSSI,BitRate,Transmission_medium]})  %% [SenderNumber,_ReceiverNumber,Rank,VersionNumber,NumberOfHops,SINR,Bitrate,Transmission_medium]
							catch
								_:Reason -> io:format("Error in sending the parsed message to the FSM process ~p~n",[Reason])
							end
					end,
					spawn(fun() -> send_dio(Fsm_Name,dio_in,Index_in) end),
					spawn(fun() -> send_rssi(SenderNumber,Fsm_Name,Index_in,Transmission_medium,N_RSSI) end),
					io:format("Incoming DIO message to ~p from ~p, medium: ~p, index: ~p~n",[Fsm_Name,SenderNumber,Transmission_medium,Index_in]),
					incoming_message(Fsm_pid,Fsm_Name);
				2 -> 
					if
						Payload_length < 7 -> Index_in = -1, io:format("error, DAO message corrupted!~n");
						true ->
							%SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
							Descendant = list_to_atom(integer_to_list(lists:nth(4,Payload))),
							Descendant_parent = list_to_atom(integer_to_list(lists:nth(5,Payload))),
							Trans_medium_Desc = lists:nth(6,Payload),
							Index_in = lists:nth(7,Payload),
							Desc_Payload = {Descendant,Descendant_parent,Trans_medium_Desc},
							%io:format("The incoming Dao message is ~p~n",[{Message_type,[SenderNumber,ReceiverNumber,Desc_Payload]}])
							try
								gen_fsm:send_event(Fsm_Name, {Message_type,[SenderNumber,ReceiverNumber,Desc_Payload]})  %{2, [_SenderNumber,ReceiverNumber,Payload]}
							catch
								_:Reason -> io:format("Error in sending the parsed message to the FSM process ~p~n",[Reason])
							end
					end,
					spawn(fun() -> send_dao(Fsm_Name,dao_in,Index_in) end),
					spawn(fun() -> send_rssi(SenderNumber,Fsm_Name,Index_in,Transmission_medium,N_RSSI) end),
					io:format("Incoming DAO message to ~p from ~p, medium: ~p, index: ~p~n",[Fsm_Name,SenderNumber,Transmission_medium,Index_in]),
					incoming_message(Fsm_pid,Fsm_Name);
				3 ->
					if
						Payload_length < 5 -> Index_in = -1, io:format("error, Data message corrupted!~n");
						true ->
							%SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
							Length_Path = lists:nth(4,Payload),
							Length_tail = length(lists:nthtail(4,Payload)),
							if
								Length_tail < Length_Path ->
									Path = [],
									io:format("error, Data message corrupted! ~n"),
									incoming_message(Fsm_pid,Fsm_Name);
								true ->
									Path = lists:sublist(Payload,5,Length_Path)
							end,
%%							try
%%								Path = lists:sublist(Payload,5,Length_Path)
%%							catch
%%									Error -> io:format("error, Data message corrupted! ~p~n",[Error]),
%%										incoming_message(Fsm_pid)
%%							end,
							Parsed_path = parsePath(Path),
							%case Parsed_path of
							%	[] -> incoming_message(Fsm_pid);
							%	_ -> ok
							%end,
							Payload_length1 = lists:nth(5+Length_Path,Payload),
							Length_Remain = length(lists:nthtail(5+Length_Path,Payload)),
							if
								Length_Remain < Payload_length1 ->
									Payload1 = [],
									io:format("error, Data message corrupted! ~n"),
									Index_in = -1,
									incoming_message(Fsm_pid,Fsm_Name);
								true ->
									UnParsed_Payload = lists:sublist(Payload,5+Length_Path+1,Payload_length1),
									Payload1 = erlang:binary_to_term(erlang:list_to_binary(UnParsed_Payload)),
									Index_in = lists:nth(6+Length_Path+Payload_length1, Payload)
							end,
							io:format("the Payload of the data message is ~p~n",[Payload1]),
							try
								gen_fsm:send_event(Fsm_Name, {Message_type,[SenderNumber,ReceiverNumber,Parsed_path,Payload1]})
							catch
								_:Reason -> io:format("Error in sending the parsed message to the FSM process ~p~n",[Reason])
							end
					end,
					spawn(fun() -> send_rssi(SenderNumber,Fsm_Name,Index_in,Transmission_medium,N_RSSI) end),
					incoming_message(Fsm_pid,Fsm_Name);
				4 ->
					if
						Payload_length =< 4 -> Index_in = -1, io:format("error, Data message corrupted!~n");
						true ->
							%SenderNumber = list_to_atom(integer_to_list(lists:nth(2,Payload))),
							ReceiverNumber = list_to_atom(integer_to_list(lists:nth(3,Payload))),
							MessageType = Message_type - 1,
							Payload_length1 = lists:nth(4,Payload),
							Length_tail = length(lists:nthtail(4,Payload)),
							if
								Length_tail < Payload_length1 ->
									Payload1 = [],
									Index_in = -1,
									io:format("error, Data message corrupted! ~n"),
									incoming_message(Fsm_pid,Fsm_Name);
								true ->
									UnParsed_Payload = lists:sublist(Payload,5,Payload_length1),
									Payload1 = erlang:binary_to_term(erlang:list_to_binary(UnParsed_Payload)),
									Index_in = lists:nth(5+Payload_length1, Payload)
							end,
%%							try
%%								Payload = lists:sublist(Payload,5,Payload_length)
%%							catch
%%			unknown message type, ignore the message!
%						Error -> io:format("error, Data message corrupted! ~p~n",[Error]),
%%									incoming_message(Fsm_pid)
%%							end,
							try
								gen_fsm:send_event(Fsm_Name, {MessageType, [SenderNumber,ReceiverNumber,-1,Payload1]})
							catch
								_:Reason -> io:format("Error in sending the parsed message to the FSM process ~p~n",[Reason])
							end
					end,
					spawn(fun() -> send_rssi(SenderNumber,Fsm_Name,Index_in,Transmission_medium,N_RSSI) end),
					incoming_message(Fsm_pid,Fsm_Name);
				
				5 -> 
					io:format("new data on demand message from the application ~n"),
					MsgType = 3,
					Data = lists:nthtail(2, Payload),
					Parsed_Data = erlang:binary_to_term(erlang:list_to_binary(Data)),
					try
						gen_fsm:send_event(Fsm_Name, {MsgType, ['1', Fsm_Name, -1, Parsed_Data]})
					catch
						_:Reason -> io:format("Error in sending the data message to the Fsm : ~p~n",[Reason])
					end,
 					incoming_message(Fsm_pid,Fsm_Name);
				_ -> io:format("unknown message type, ignore the message: ~p~n",[Message]),
						incoming_message(Fsm_pid,Fsm_Name)
			end
			%spawn(fun() -> send_rssi(SenderNumber,Fsm_Name,Transmission_medium,RSSI) end)
	end.

outmessage({Message_type, [_SenderNumber, _ReceiverNumber, _Rank, _VersionNumber,_NumberOfHops, Transmission_medium]}) when Message_type =:= 1 -> %% sending DIO message
	Value = get(count),
	case Value of
		undefined -> Random_Num = random:uniform(255),
			     put(count,Random_Num),
			     Count = Random_Num;
		_ -> 
			if
			  Value =:= 255 -> put(count,0),
					Count = 0;
			  true -> put(count,Value+1),
				  Count = Value+1
			end
	end,			
	io:format("Outgoing DIO message from ~p, index: ~p~n",[_SenderNumber,Count]),
	%Rest = randbytes(57),
	%Length_Rest = Rest*8,
	%Zeros = <<0:Length_Rest>>,
	case _Rank of
		infinity -> Rank1 = trunc(math:pow(2,16))-1,
					NumOfHops = trunc(math:pow(2,8))-1;
		_ -> Rank1 = _Rank,
			 NumOfHops = _NumberOfHops
	end,
	Rank = <<Rank1:16>>,
	%io:format("The SenderNumber is ~p and its type is an atom?: ~p, the ReceiverNumber is ~p and its type is an atom?: ~p ~n", [_SenderNumber,is_atom(_SenderNumber),_ReceiverNumber,is_atom(_ReceiverNumber)]),
	Payload = [Message_type,list_to_integer(atom_to_list(_SenderNumber)),list_to_integer(atom_to_list(_ReceiverNumber))]++bitstring_to_list(Rank)++[_VersionNumber,NumOfHops,Count], %++bitstring_to_list(Zeros),
	%io:format("The Payload of the outcoming DIO message from station ~p is ~p~n",[_SenderNumber,Payload]),
	spawn(fun() -> send_dio(_SenderNumber,dio_out,Count) end),
	?ModemModule:send(Transmission_medium,Payload);

outmessage({Message_type,[SenderNumber,ReceiverNumber,Transmission_medium,Payload]}) when Message_type =:= 2 -> %% sending DAO message
	Value = get(count),
	case Value of
		undefined -> Random_Num = random:uniform(255),
			     put(count,Random_Num),
			     Count = Random_Num;
		_ -> 
			if
			  Value =:= 255 -> put(count,0),
					Count = 0;
			  true -> put(count,Value+1),
				  Count = Value+1
			end
	end,	
	io:format("Outgoing DAO message from ~p to station: ~p via medium: ~p, index: ~p~n",[SenderNumber,ReceiverNumber,Transmission_medium,Count]),
	{Descendant,Descendant_parent,_Transmission_medium} = Payload,
	%Rest = randbytes(57),
	%Length_Rest = Rest*8,
	%Zeros = <<0:Length_Rest>>,
	Payload_parse = [Message_type,list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber)),list_to_integer(atom_to_list(Descendant)),list_to_integer(atom_to_list(Descendant_parent)),_Transmission_medium,Count], %++bitstring_to_list(Zeros),
	%io:format("The Payload of the outcoming DAO message from station ~p is ~p~n",[SenderNumber,Payload_parse]),
	spawn(fun() -> send_dao(SenderNumber,dao_out,Count) end),
	?ModemModule:send(Transmission_medium,Payload_parse);

outmessage({Message_type, [SenderNumber,ReceiverNumber,Path,Payload], _PTransmission_medium}) when Message_type =:= 3 -> %% Data messages
	Value = get(count),
	case Value of
		undefined -> Random_Num = random:uniform(255),
			     put(count,Random_Num),
			     Count = Random_Num;
		_ -> 
			if
			  Value =:= 255 -> put(count,0),
					Count = 0;
			  true -> put(count,Value+1),
				  Count = Value+1
			end
	end,		
	case Path of
		-1 ->
			Type_of_message = Message_type + 1,  %% if there is no path, the type of message will be 4 in order to distinguish between data messages with path and without
			Parsed_Data = erlang:binary_to_list(erlang:term_to_binary(Payload)),
			Data_length = length(Parsed_Data),
			%Binary_Payload_Size = Payload_length*8,
			%Rest = randbytes(61 - Payload_length - 1),
			%Length_Rest = Rest*8,
			%Zero_Padding_Payload = <<0:Length_Rest>>,
			Payload_parse = [Type_of_message]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Data_length]++Parsed_Data++[Count], %++bitstring_to_list(Zero_Padding_Payload),
			%io:format("The Payload of the outcoming message is ~p~n",[Payload_parse]);
			?ModemModule:send(_PTransmission_medium,Payload_parse);
		_ ->
			Parsed_path = convert_path(Path),
			Length_Path = length(Parsed_path),
			Parsed_Data = erlang:binary_to_list(erlang:term_to_binary(Payload)),
			Data_length = length(Parsed_Data),
			Payload_parse = [Message_type]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber)),Length_Path]++Parsed_path++[Data_length]++Parsed_Data++[Count],
			%Path_size = Length_Path*8,
			%IsList = is_list(Payload),
			%if
			%	IsList =:= true -> Payload_length = length(Payload);
			%	true -> Payload_length = 1
			%end,
			%Payload_size = Payload_length*8,
			%Rest = randbytes(61-Payload_length-Length_Path-1),
			%Length_Rest = Rest*8,
			%Zero_Padding_Payload = <<0:Length_Rest>>,
			%%Parsed_path = convert_path(Path),
			%%Zero_Padding_Payload = <<Payload:Payload_size>>,
			%if
			%	IsList =:= true -> Payload_parse = [Message_type]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Length_Path]++Parsed_path++[Payload_length]++Payload++[Count]; %++bitstring_to_list(Zero_Padding_Payload);
			%	true -> Payload_parse = [Message_type]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Length_Path]++Parsed_path++[Payload_length]++[Payload]++[Count] %++bitstring_to_list(Zero_Padding_Payload)
			%end,
			%Payload_parse = [Message_type]++[list_to_integer(atom_to_list(SenderNumber)),list_to_integer(atom_to_list(ReceiverNumber))]++[Length_Path]++Parsed_path++[Payload_length]++Payload++bitstring_to_list(Zero_Padding_Payload),
			%%io:format("The Payload of the outcoming message is ~p~n",[Payload_parse])
			?ModemModule:send(_PTransmission_medium,Payload_parse)
	end;
	%spawn(fun() -> send_data_stat(SenderNumber,Payload) end);

outmessage(Message) -> 
		io:format("Out-message: ~p with wrong pattern! ~n", [Message]).

parsePath(Path) -> parsePath(Path,[]). 
parsePath(Path,List) when Path =:= [] -> lists:reverse(List);
parsePath(Path,List) ->
	Path_Length = length(Path),
	if
		Path_Length >= 3 ->
			Descendant = list_to_atom(integer_to_list(lists:nth(1,Path))),
			Descendant_parent = list_to_atom(integer_to_list(lists:nth(2,Path))),
			Trans_medium_Desc = lists:nth(3,Path),
			Remaining_Path = lists:nthtail(3,Path),
			Next_Stop = {Descendant,Descendant_parent,Trans_medium_Desc},
			parsePath(Remaining_Path,[Next_Stop]++List);
		true ->
	    	io:format("error in path's parsing, incomplete path is returned ~n"),
				lists:reverse(List)
	end.


%% the function below convert the path into a list of numbers
convert_path(Path) -> convert_path(Path,[]).
convert_path(Path,List) when Path =:= [] -> lists:flatten(lists:reverse(List));
convert_path(Path,List) ->
	{NewReceiver,StationNumber,Transmission_medium} = lists:nth(1,Path),
	convert_path(lists:nthtail(1,Path),[[list_to_integer(atom_to_list(NewReceiver)),list_to_integer(atom_to_list(StationNumber)),Transmission_medium]]++List).

%% randomize a number in the range between 1 to Number
% randbytes(Number) ->
% 	random:uniform(Number).

send_message_Fsm(Message) ->
	%io:format("send message to parsing process ~n"),
	try
		parseMessageIn ! Message
	catch
		_:Reason -> io:format("can't send your message to the parser of incoming messages, Error: ~p ~n",[Reason])
	end.

-spec start(StationNumber :: atom()) -> FSM_pid :: pid().
start(StationNumber) -> 
    case StationNumber of
      '1' -> root:start(StationNumber);
       _ -> station:start(StationNumber)
    end.

send_dio(StationNumber,Atom,Index) -> %ok.
	%io:format("~p message, StationNumber ~p Index ~p~n",[Atom,StationNumber,Index]),
	?LocalServer:send({Atom,StationNumber,Index}).

send_dao(StationNumber,Atom,Index) -> %ok.
	%io:format("~p message, StationNumber ~p Index ~p~n",[Atom,StationNumber,Index]),
	?LocalServer:send({Atom,StationNumber,Index}).	

send_data_stat(StationNumber,Payload) -> 
	?LocalServer:send({data,StationNumber,Payload}).


send_rssi(SenderNumber,Fsm_Name,Index,Transmission_medium,Rssi) -> %ok.
	% case Transmission_medium of
	% 	1 -> Medium = rf;
	% 	2 -> Medium = plc;
	% 	_ -> Medium = unknown, io:format("wrong Transmission_medium ~n")
	% end,
	%io:format("The Rssi of the incoming message via medium: ~p to SenderNumber ~p is ~p ~n",[Medium,Fsm_Name,Rssi]),
	?LocalServer:send({rssi,Fsm_Name,SenderNumber,Index,Transmission_medium,Rssi}).

up_data_counter_origen() -> 
	?LocalServer:send(counter_origen_up).

receiver_count_up() -> 
	?LocalServer:send(counter_receiver_up).

send_data(Data) ->
	ParsedData = erlang:binary_to_list(erlang:term_to_binary(Data)),
	DataList = [1,1,5,1] ++ ParsedData,
	send_message_Fsm(DataList).
