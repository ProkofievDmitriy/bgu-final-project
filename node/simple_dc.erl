-module(simple_dc).
-export([start_link/1, stop/0]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


stop()->
    ?APPLICATION_NAME ! stop.


start_link(Properties)->
	?LOGGER:info("[~p]: Starting Simple Data Concentration Application with props: ~w~n", [?MODULE, Properties]),
	PID = spawn(fun()-> data_concentration_loop() end),
	register(?APPLICATION_NAME, PID),
    ?PROTOCOL:hand_shake(PID),
	PID.

data_concentration_loop()->
	receive
		stop ->
		    ?LOGGER:info("[~p]: Received stop message. Exiting data concentration server App~n", [?MODULE]),
		    normal;
		Message ->
            {From, NewMessage} = binary_to_term(Message),
            ?LOGGER:info("[~p]: successfully received message length=~p , Message : ~p  ~n", [?MODULE, length(NewMessage), NewMessage]),
            ?PROTOCOL:send(utils:get_node_number(From), term_to_binary("Message successfully delivered!")),
            data_concentration_loop()
	end.
