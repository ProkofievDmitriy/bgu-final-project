-module(simple_app).
-export([start/1, stop/0]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


stop()->
    ?APPLICATION_NAME ! stop.


start(Properties)->
	?LOGGER:info("[~p]: Starting Simple Application with props: ~w~n", [?MODULE, Properties]),
    Role = proplists:get_value(role, Properties),
	case Role of
		smart_meter ->
	        SendInterval = proplists:get_value(send_message_interval, Properties),
            PID = spawn(fun()->smart_meter_loop(SendInterval, 1000) end),
            ?LOGGER:info("[~p]: ~p  mode started~n", [?MODULE, Role]),
            register(?APPLICATION_NAME, PID),
            PID;
		data_concentration_server ->
            ?LOGGER:info("[~p]: ~p  mode started~n", [?MODULE, Role]),
			PID = spawn(fun()-> data_concentration_loop() end),
			register(?APPLICATION_NAME, PID),
			PID;
		_else ->
		    ?LOGGER:error("[~p]: not supported role : ~p~n", [?MODULE, Role]),
		    not_supported_role_error
	end.

data_concentration_loop()->
	receive
		stop ->
		    ?LOGGER:info("Received stop message. Exiting data concentration server App~n"),
		    normal;
		{Source, Data}->
	        ?LOGGER:info("Received message from ~p, data: ~p~n", [Source, Data]),
            data_concentration_loop()
	end.



smart_meter_loop(SendInterval, FalseLoops) ->
    receive
        stop ->
		    ?LOGGER:info("[~p]: Received stop message. Exiting ... ~n", [?MODULE]),
            normal;
         Message ->
            ?LOGGER:info("[~p]: Received message: ~p  ~n", [?MODULE, Message]),
            ok
        after ?MESSAGE_SEND_INTERVAL ->
            case FalseLoops rem 1000 of
                0 ->
                Destination = 1,
                Data = "some message",
                ?LOGGER:info("[~p]: Sending Message ~p~n" ,[?MODULE, {Destination, Data}]),
                Result = ?PROTOCOL:send({Destination, Data}),
                case Result of
                        {error, Message} ->
                            ?LOGGER:info("[~p]: Received error message : ~p ~n" ,[?MODULE, Message]);
                        sent ->
                            ?LOGGER:info("[~p]: Message successfully sent!!!~n" ,[?MODULE]);
                        SomeResult ->
                            ?LOGGER:info("[~p]: Some unexpected result received: ~p~n" ,[?MODULE, SomeResult])
                end;
                _Else -> ok
            end,
            smart_meter_loop(SendInterval, FalseLoops + 1)
    end.


