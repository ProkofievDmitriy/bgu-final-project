-module(stubNode).
-behaviour(gen_server).




-export([start/1, stop/0, simulate/0]).

-export([sendMsg/2, chageState/2, sendDataMsg/2, receivedDataMsg/2, sendMangMsg/2, receivedMangMsg/2]).
-record(state, {name, rf, plc}).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
  


simulate() ->
	io:format("Starting simulation~n"),
	spawn(fun run_simulate/0).

run_simulate() -> 
	receive
		stop->ok
	after 50000 ->
		R = rand:uniform(4),
		case R of
			0 -> sendDataMsg(1,"AAA");
			1 -> receivedDataMsg("BBB",2);
			2 -> sendMangMsg("CCC",3);
			3 -> receivedMangMsg("DDD",4);
			_ -> ok
		end,
		run_simulate()
	end.

sendMsg(To,Msg)->
	io:format("Send Msg: ~p, TO: ~p~n",[Msg,To]),
	gen_server:cast(?MODULE, {sendMSG, To,Msg}).



chageState(PLC,RF)->
	io:format("Change State: PLC ~p, RF ~p~n",[PLC,RF]),
	gen_server:cast(?MODULE, {state, PLC,RF}).


sendDataMsg(Msg,To)->
	io:format("Sending Msg ~p to ~p~n",[Msg,To]),
	gen_server:cast(?MODULE, {sent_data_message, Msg, To}).

receivedDataMsg(Msg,From)->
	io:format("Received data Msg ~p From ~p~n",[Msg,From]),
	gen_server:cast(?MODULE, {received_data_message, Msg, From}).



sendMangMsg(Msg,To)->
	io:format("Sending Msg ~p to ~p~n",[Msg,To]),
	gen_server:cast(?MODULE, {sent_management_message, Msg, To}).

receivedMangMsg(Msg,From)->
	io:format("Received data Msg ~p from ~p~n",[Msg,From]),
	gen_server:cast(?MODULE, {received_management_message, Msg, From}).







%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
start_link(Name) ->
	io:format("BStarting ~p~n",[Name]),
    gen_server:start_link({local,?MODULE}, ?MODULE, [Name], []).

start(Name) ->
	io:format("AStarting ~p~n",[Name]),
    start_link(Name).
    
stop() -> 
  gen_server:cast(?MODULE, stop).


init([Name]) ->
    io:format("initiating server ~p~n",[Name]),
	erlang:send_after(5000,self(),timer),
    {ok, #state{name = Name, plc = 0, rf = 1}}.

handle_call(Req, From, State) ->
    io:format("stats_server handle_call: ~p from ~p~n", [Req,From]),
    {reply, ignored, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast({received_management_message, Msg, From}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p from ~p~n", [Name, Msg, From]),
	stats_server_interface:received_management_message(Name, From, Msg),
    {noreply, State};

handle_cast({sent_management_message, Msg, To}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p to ~p~n", [Name, Msg,To]),
	stats_server_interface:sent_management_message(Name, To, Msg),
    {noreply, State};

handle_cast({received_data_message, Msg, From}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p from ~p~n", [Name, Msg,From]),
	stats_server_interface:received_data_message(Name, From, Msg),
    {noreply, State};

handle_cast({sent_data_message, Msg, To}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p to ~p~n", [Name, Msg,To]),
	stats_server_interface:sent_data_message(Name, To, Msg),
    {noreply, State};

handle_cast({state, PLC, RF}, State = #state{name = Name}) -> 
	io:format("~p changing State: PLC ~p, RF ~p~n",[Name, PLC,RF]),
    {noreply, State#state{plc = PLC, rf = RF}};


handle_cast({sendMSG, To,Msg}, State = #state{name = Name}) -> 
	io:format("~p sending Msg: ~p, TO: ~p~n",[Name, Msg,To]),
    {noreply, State};




handle_cast(Msg, State) -> 
    io:format("stats_server got cast with bad arg:~p~n", [Msg]),
    {noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(stop, State) ->
{stop, stop, State};




handle_info(timer, State#{name = Name, plc = PLC, rf = RF) ->
  io:format("stubNode: timer~n"),
      stats_server_interface:node_is_up(Name,[{medium_mode,{PLC,RF}},{routing_set,[]}]),
  erlang:send_after(5000,self(),timer),
  {noreply, State};


handle_info(Info, State) ->
  io:format("isg_server:handle_info: got somethjing:~p~n", [Info]),
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	io:format("isg server terminated!!!~n"),
  ok.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



