-module(simple_app).
-export([start/1, stop/0]).

-include("./include/properties.hrl").



stop()->
    ?APPLICATION_NAME ! stop.


start(Properties)->
	?LOGGER:info("[~p]: Starting Simple Application with props: ~p~n", [?MODULE, Properties]),
    Role = proplists:get_value(role, Properties),
	case Role of
		smart_meter ->
	        SendInterval = proplists:get_value(send_message_interval, Properties),
            PID = spawn(fun()->smart_meter_loop(SendInterval) end),
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



smart_meter_loop(SendInterval) ->
    receive
        stop->
		    ?LOGGER:info("Received stop message. Exiting smart meter App~n"),
            normal
        after ?MESSAGE_SEND_INTERVAL ->
            Destination = "some destination",
            Data = " some message",
            Headers = [],
            ?PROTOCOL:send({Destination, Headers, Data}),
            smart_meter_loop(SendInterval)
    end.


