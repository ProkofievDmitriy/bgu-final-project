-module(simple_app).
-export([start/2, stop/0]).

-include("./include/properties.hrl").


%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(PROTOCOL, load_ng).
-define(MESSAGE_SEND_INTERVAL, 5000). % 5 seconds interval between messages


stop()->
    ?APPLICATION_NAME ! stop.


start(Role, Node_Id)->
	?LOGGER:info("Starting Simple Application in role: ~p~n", [Role]),
	case Role of
		smart_meter ->
            Pid = spawn(fun()->smart_meter_loop(Node_Id) end),
            ?LOGGER:info("~p ~p  mode started~n", [?MODULE, Role]),
            register(?APPLICATION_NAME, Pid),
            Pid;
		data_concentration_server ->
            ?LOGGER:info("~p ~p  mode started~n", [?MODULE, Role]),
			Sender_PID = spawn(fun()-> data_concentration_loop(Node_Id) end),
			register(app,Sender_PID), Sender_PID;
		_else ->
		    ?LOGGER:error("~p not supported role : ~p~n", [?MODULE, Role]),
		    ok
	end.

data_concentration_loop(Node_Id)->
	receive
		stop ->
		    ?LOGGER:info("Received stop message. Exiting data concentration server App~n"),
		    normal;
		{Source, Data}->
	        ?LOGGER:info("Received message from ~p, data: ~p~n", [Source, Data]),
            data_concentration_loop(Node_Id)
	end.



smart_meter_loop(Node_Id) ->
    Destination = "some destination",
    Data = " some message",
    ?PROTOCOL:send({Destination, Data}),
    receive
        stop->
		    ?LOGGER:info("Received stop message. Exiting smart meter App~n"),
            normal
        after ?MESSAGE_SEND_INTERVAL ->
            smart_meter_loop(Node_Id)
    end.


