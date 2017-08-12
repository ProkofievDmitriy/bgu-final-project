-module(simple_dc).
-export([start_link/1, stop/0]).
-export([start_from_gui/1, configuration_updated_from_gui/2, routing_tables_cleared_from_gui/1, stations_removed_from_gui/1]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


stop()->
    ?MODULE ! stop.


start_link(Properties)->
	?LOGGER:info("[~p]: Starting Simple Data Concentration Application with props: ~w~n", [?MODULE, Properties]),
	PID = spawn(fun()-> data_concentration_loop() end),
	register(?MODULE, PID),
    ?PROTOCOL:hand_shake(PID),
	PID.

data_concentration_loop()->
	receive
		stop ->
		    ?LOGGER:info("[~p]: Received stop message. Exiting data concentration server App~n", [?MODULE]),
		    normal;
		Message ->
            ?LOGGER:info("[~p]: successfully received message length=~p , Message : ~p  ~n", [?MODULE, bit_size(Message), Message]),
            TermMessage = binary_to_term(Message),
            case TermMessage of
                {From, NewMessage} ->
                    ?PROTOCOL:send(utils:get_node_number(From), term_to_binary("Message successfully delivered!")),
                    ?LOGGER:info("[~p]: Message : ~p  ~n", [?MODULE, NewMessage]),
                    data_concentration_loop();
                Other ->
                    ?LOGGER:info("[~p]: Other Message : ~p  ~n", [?MODULE, Other])
            end
	end.



start_from_gui(_)-> ok.
configuration_updated_from_gui(_, _)-> ok.
routing_tables_cleared_from_gui(_)-> ok.
stations_removed_from_gui(_)-> ok.
