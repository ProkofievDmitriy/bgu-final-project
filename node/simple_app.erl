-module(simple_app).
-export([start_link/1, stop/0]).

-include("./include/properties.hrl").
-include("./include/vcb.hrl").


stop()->
    ?APPLICATION_NAME ! stop.


start_link(Properties)->
	?LOGGER:info("[~p]: Starting Simple Smart Meter Application with props: ~w~n", [?MODULE, Properties]),
    PID = spawn(fun()->smart_meter_loop(?MESSAGE_SEND_INTERVAL, 1000, term_to_binary("Some Random Message")) end),
    register(?APPLICATION_NAME, PID),
    ?PROTOCOL:hand_shake(PID),
    PID.

smart_meter_loop(SendInterval, FalseLoops, Data) ->
    receive
        stop ->
		    ?LOGGER:info("[~p]: Received stop message. Exiting ... ~n", [?MODULE]),
            normal;
         Message ->
             NewMessage = binary_to_term(Message),
             ?LOGGER:info("[~p]: Received message length=~p bytes: ~w  ~n", [?MODULE, length(NewMessage), NewMessage]),
            ok
        after ?MESSAGE_SEND_INTERVAL ->
            case FalseLoops rem 1000 of
                0 ->
                Destination = 1,
                ?LOGGER:info("[~p]: Sending Message bit_size = ~p, Message: ~p ~n" ,[?MODULE, bit_size(Data), {Destination, Data}]),
                Result = ?PROTOCOL:send(Destination, Data),
                case Result of
                        {error, Message} ->
                            ?LOGGER:info("[~p]: Received error message : ~p ~n" ,[?MODULE, Message]);
                        {ok, sent} ->
                            ?LOGGER:info("[~p]: Message successfully sent!!!~n" ,[?MODULE]),
                            % smart_meter_loop(SendInterval, FalseLoops + 1 , Data ++ [2]);
                           smart_meter_loop(SendInterval, FalseLoops + 1 , Data);

                        SomeResult ->
                            ?LOGGER:info("[~p]: Some unexpected result received: ~p~n" ,[?MODULE, SomeResult])
                end;
                _Else -> ok
            end,
            smart_meter_loop(SendInterval, FalseLoops + 1 , Data)
    end.
