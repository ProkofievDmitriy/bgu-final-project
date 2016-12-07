-module(app).
-export([start/1, stop/0, send/1]).
-define(Sample_Time_Seconds, 120).
-define(MAXPower, 100).
-define(MAXTemp, 20).
-define(MAXHumid, 100).

stop()->app!stop.

send({data, _Data} = S) -> 
	app!S.

wait()->
		A = os:timestamp(), random:seed(A), B = random:uniform(50),
			receive after B * 1000 -> continue end. %for diffrent data sending time for every node

start(ID)->
	io:format("^^^^^Starting App!!!!!^^^^^^^~n"),
	case ID of
		ID1 when ID1=:=1 -> 
				Rec_Pid = spawn(fun()->receiver_loop() end),
				io:format("receiver mode~n"),
				register(app,Rec_Pid), Rec_Pid;
		_Else -> 
			A = os:timestamp(), random:seed(A), RandPower = random:uniform(?MAXPower),
			wait(),
			io:format("sender mode~n"),
			Sender_PID = spawn(fun()-> sender_loop(ID, ?Sample_Time_Seconds, {RandPower, 0,0}) end),
			register(app,Sender_PID), Sender_PID
	end.


extract_data(Data) when is_list(Data)==false -> {error,error, error};
extract_data([Power, Temperature, Humid, ID ]) -> {Power, Temperature, Humid, ID};
extract_data(Else) -> io:format("extract_data failes. got somthing else:~p~n", [Else]).

receiver_loop()-> 
	receive
		stop -> io:format("got stop msg. Exiting receiver App~n"), normal;
		{data, Data} -> { Power, Temperature, Humid, ID } = extract_data(Data),
				os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'data_msgs value=1'"),
				io:format("App got Data From ~p!!!!!~nMess is- Power:~p, Temperature:~p, Humid:~p~n~n", [ID, Power, Temperature, Humid ]),
				receiver_loop()
	end.




sender_loop(ID, I, {LPow, _LTemp, _LHumid} = _Last_mess) when I =:= 0->
		io:format("good morning!! taking sample......~n"),
		Power = get_random_power_mess(LPow),
		Temperature = get_random_temp_mess(),
		Humid = get_random_humid_mess(),
		io:format("************************************~nMessurment for ID:~p are -  Power:~p, Temp:~p, Humid:~p~n****************************~n", [ID,Power, Temperature, Humid]),
		MSG = preper_data_to_send(Power, Temperature, Humid, ID),
		hyRPL:send_data(MSG),
		os:cmd("curl -i -XPOST 'http://localhost:8086/write?db=mydb' --data-binary 'data_msgs value=0'"),	
		sender_loop(ID, ?Sample_Time_Seconds, {Power, Temperature, Humid});
sender_loop(ID, I, Last_mess) -> receive
						stop-> io:format("got stop msg. Exiting sender App~n"), normal
						after 1000 -> sender_loop(ID, I-1, Last_mess) end.


get_random_power_mess(LPow) when LPow ==100 -> A = os:timestamp(), random:seed(A), random:uniform(?MAXPower);
get_random_power_mess(LPow)-> A = os:timestamp(), random:seed(A), B = random:uniform(6), LPow -3 + B.

get_random_temp_mess()-> A = os:timestamp(), random:seed(A), 20 + random:uniform(?MAXTemp).

get_random_humid_mess()-> A = os:timestamp(), random:seed(A), random:uniform(?MAXHumid).

preper_data_to_send(Power, Temperature, Humid, ID)-> [Power] ++ [Temperature] ++ [Humid] ++ [ID].
