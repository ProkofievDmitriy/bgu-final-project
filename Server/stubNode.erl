-module(stubNode).
-behaviour(gen_server).




-export([start/0, stop/0, simulate/1]).

-export([deleteTable/0, sendMsg/2, chageState/1, addRoute/2]).
-record(state, {name, medium_mode, routing_set}).
-record(routing_set_entry, {dest_addr, next_addr, medium}).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
  


simulate(ID) ->
	io:format("Starting simulation~n"),
	case ID of
		1 ->
			chageState(rf_only),
			addRoute('N2@127.0.0.1', 1);
		2 ->chageState(dual),
			addRoute('N1@127.0.0.1', 1),
			addRoute('N3@127.0.0.1', 2);
		3 -> chageState(plc_only),
			addRoute('N2@127.0.0.1', 2)
	end,
	spawn(fun run_simulate/0).

run_simulate() -> 
	receive
		stop->ok
	after 5000 ->
		R = rand:uniform(4),
		case R of
			1 -> sendDataMsg(1,2,3);
			2 -> receivedDataMsg(1,2,3);
			3 -> sendMangMsg(1,2,3);
			4 -> receivedMangMsg(1,2,3);
			_ -> io:format("~n~nUnhandeld R!~n~n")
		end,
		run_simulate()
	end.

addRoute(Node, Medium)->
	io:format("Add route to ~p, with: ~p~n",[Node,Medium]),
	gen_server:cast(?MODULE, {addRoute, Node, Medium}).



sendMsg(To,Msg)->
	io:format("Send Msg: ~p, TO: ~p~n",[Msg,To]),
	gen_server:cast(?MODULE, {sendMSG, To,Msg}).


deleteTable()->
	io:format("delete Table~n"),
	gen_server:cast(?MODULE, {deleteTable}).



chageState(Medium_mode)->
	io:format("Change State: Medium_mode ~p~n",[Medium_mode]),
	gen_server:cast(?MODULE, {state, Medium_mode}).


sendDataMsg(Source, Destination,Id)->
	io:format("Sending Data sMsg From ~p To ~p Msg ~p~n",[Source, Destination,Id]),
	gen_server:cast(?MODULE, {sent_data_message, Source, Destination,Id}).

receivedDataMsg(Source, Destination,Id)->
	io:format("Received data From ~p To ~p Msg ~p~n",[Source, Destination,Id]),
	gen_server:cast(?MODULE, {received_data_message, Source, Destination,Id}).



sendMangMsg(Source, Destination,Type)->
	io:format("Sending mang Msg From ~p To ~p Msg ~p~n",[Source, Destination,Type]),
	gen_server:cast(?MODULE, {sent_management_message, Source, Destination,Type}).

receivedMangMsg(Source, Destination,Type)->
	io:format("Received mang From ~p To ~p Msg ~p~n",[Source, Destination,Type]),
	gen_server:cast(?MODULE, {received_management_message, Source, Destination,Type}).









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

start() ->
	net_kernel:connect('stats_server@127.0.0.1'),
	Name = node(),
	io:format("AStarting ~p~n",[Name]),
	
    start_link(Name).
    
stop() -> 
  gen_server:cast(?MODULE, stop).


init([Name]) ->
    io:format("initiating server ~p~n",[Name]),
	erlang:send_after(5000,self(),timer),
    {ok, #state{name = Name, medium_mode = idle, routing_set = []}}.

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

handle_cast({received_management_message, Source, Destination,Type}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p from ~p~n", [Name, Type, Source]),
	%stats_server_interface:received_management_message(Name, From, Msg),
	stats_server_interface:report({management_message,received_message},  [{source, Source},
 											   			{destination, Destination},
										   				{message_type, Type}]),
    {noreply, State};

handle_cast({sent_management_message, Source, Destination,Type}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p to ~p~n", [Name, Type,Destination]),
	%stats_server_interface:sent_management_message(Name, To, Msg),
	stats_server_interface:report({management_message,send_message}, [{source, Source},
 											   			{destination, Destination},
										   				{message_type, Type}]),
    {noreply, State};

handle_cast({received_data_message, Source, Destination,Id}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p from ~p~n", [Name, Id,Destination]),
	%stats_server_interface:received_data_message(Name, From, Msg),
	stats_server_interface:report({data_message,received_message}, [{source, Source},
									   			{destination, Destination},
								   				{id, Id}]),

    {noreply, State};

handle_cast({sent_data_message, Source, Destination,Id}, State = #state{name = Name}) -> 
    io:format("~p Sending Msg ~p to ~p~n", [Name, Id,Destination]),
	%stats_server_interface:sent_data_message(Name, To, Msg),
	stats_server_interface:report({data_message,send_message}, [{source, Source},
									   			{destination, Destination},
								   				{id, Id}]),

    {noreply, State};

handle_cast({state, Medium_mode}, State = #state{name = Name}) -> 
	io:format("~p changing State: Medium_mode: ~p~n",[Name, Medium_mode]),
    {noreply, State#state{medium_mode = Medium_mode}};



handle_cast({deleteTable}, State = #state{name = Name}) -> 
	io:format("~n~p deleting Table!~n~n",[Name]),
    {noreply, State};


handle_cast({sendMSG, To,Msg}, State = #state{name = Name}) -> 
	io:format("~p sending Msg: ~p, TO: ~p~n",[Name, Msg,To]),
    {noreply, State};

handle_cast({addRoute, Node, Medium}, State = #state{name = Name, routing_set = Routing_set}) -> 
	io:format("Adding: ~p, with: ~p~n",[Node, Medium]),
	NewRS = [#routing_set_entry{dest_addr = Node, next_addr = Node, medium = Medium}|Routing_set],
    {noreply, State#state{routing_set = NewRS}};




handle_cast(stop, State) -> 
    io:format("STOP~n"),
    stats_server_interface:node_is_down(State#state.name),
    {stop, normal,State};

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




handle_info(timer, State = #state{name = Name, medium_mode = Medium_mode, routing_set = Routing_set}) ->
  io:format("stubNode: timer~n"),
  %stats_server_interface:node_is_up(Name,[{medium_mode,{PLC,RF}},{routing_set,Routing_set}]),
  stats_server_interface:report(node_state, [{node_name,Name},{medium_mode,Medium_mode},{routing_set,Routing_set}]),
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



