-module(loadNGgui).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(SERVER, ?MODULE).
-define(X_SIZE, 1080).
-define(Y_SIZE, 700).
-define(REFRESH_TIME, 1000).
-define(TIMEOUT, 7000).

-define( LOG_DIR,"./logger/").
-define(CIRCE_RADIUS, 15).
-define(CIRCE_RADIUS_SQURE, ?CIRCE_RADIUS*?CIRCE_RADIUS).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2,handle_cast/2, handle_call/3, handle_event/2, handle_sync_event/3]).

-record(state,
	{frame,panel, mapEts, nodesEts, canvas, log, nodeChoice, selectedNode = all, numberOfNodes, buttons,%}).
%-record(buttons, {
    counters, configButtons, updateLocation,
    buttonExport, buttonFullMap, buttonDeleteTable, buttonDeleteAll,
	buttonSendConfig, buttonSendMSG, txtMsgSend,
	buttonUpdateNodesToFilter, nodesToFilterList, buttonStartApp, buttonRemoveNode,
	cmbTo}).

-record(counters, {numberOfRelayMsg, numberOfManagementMsgSent, numberOfManagementMsgReceived, numberOfDataMsgSent,
                    numberOfDataMsgReceived, data_msg_avg_time, data_msg_avg_relay_length}).

%%%%%%%%%%%%
%%%     MapEts: {{NodeNameAtom,NextNode},{Medium}}
%%%
%%%     NodesEts: {NodeNameAtom,{MediumMode,RoutingSet}}
%%%%%%%%%%%%
start() ->
    io:format("start 1 ~n"),
    WxServer = wx:new(),
    io:format("Server: ~p~n",[WxServer]),
    stats_server:start(),
    wx_object:start_link({global, ?SERVER}, ?MODULE, WxServer, []).


init(WxServer) ->
    NodesEts = ets:new(nodesEts,[set,named_table]),
    MapEts = ets:new(mapEts,[set,named_table]),  % {NodeNameAtom,{Location, Medium ,RoutingSet}}
		io:format("init 1 ~n"),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%                          GUI Setup:                          %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%.

    Frame = wxFrame:new(WxServer, ?wxID_ANY, "Smart Meter Network Management Tool", [{size,{?X_SIZE, ?Y_SIZE+40}}]),
    io:format("Frame: ~p~n",[Frame]),
    Panel = wxPanel:new(Frame),

    SuperSz = wxBoxSizer:new(?wxVERTICAL),
    TitleSz = wxBoxSizer:new(?wxVERTICAL),
    OuterSz  = wxBoxSizer:new(?wxHORIZONTAL),
    LogSz = wxBoxSizer:new(?wxVERTICAL),


    NodesSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Nodes:"}]),
    ManagementSz = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Management:"}]),
    ManagementSzLeftP = wxBoxSizer:new(?wxVERTICAL),
    ManagementSzRightP = wxBoxSizer:new(?wxVERTICAL),
    LeftP = wxBoxSizer:new(?wxVERTICAL),

    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, ManagementSzLeftP),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, ManagementSzRightP),

    %% create widgets
    MikraPLC = wxStaticText:new(Panel, ?wxID_ANY," PLC |",[{style, ? wxALIGN_RIGHT}]),
    MikraRF = wxStaticText:new(Panel, ?wxID_ANY," RF   |",[{style, ? wxALIGN_RIGHT}]),
    Counters = wxStaticText:new(Panel, ?wxID_ANY,"Stats Server is loading",[{style, ? wxALIGN_LEFT}]),

    wxStaticText:setForegroundColour(MikraPLC, ?wxBLUE),
    wxStaticText:setForegroundColour(MikraRF, ?wxRED),
    Title = wxStaticText:new(Panel, ?wxID_ANY,"Smart Meter Network Management Tool:",[{style, ?wxALIGN_CENTER}]),

    %%Setup Buttons:
    ButtonDeleteTable = wxButton:new(Panel, ?wxID_ANY, [{label,"Clear Node Routing Table"}]),
    wxButton:connect(ButtonDeleteTable, command_button_clicked),

	ButtonDeleteAll = wxButton:new(Panel, ?wxID_ANY, [{label,"Clear All Nodes"}]),
    wxButton:connect(ButtonDeleteAll, command_button_clicked),

	%CmbTo = wxChoice:new(Panel, ?wxID_ANY, [{choices, []},{style, ?wxCB_READONLY}]),
	CmbTo = wxChoice:new(Panel, ?wxID_ANY,[{size,{170,40}}]),
    TxtMsgSend = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "Default Message"}, {size,{170,130}},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

	ButtonSendMSG = wxButton:new(Panel, ?wxID_ANY, [{label, "Send Message"}]),
    wxButton:connect(ButtonSendMSG, command_button_clicked),


	NodesToFilterTxt = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "Accept All"}, {size,{170,25}},{style, ?wxDEFAULT}]),
	ButtonUpdateNodesToFilter = wxButton:new(Panel, ?wxID_ANY, [{label, "Update Nodes To Filter List"}]),
	wxButton:connect(ButtonUpdateNodesToFilter, command_button_clicked),


    ButtonStartApp = wxButton:new(Panel, ?wxID_ANY, [{label,"Start Application"}]),
	wxButton:connect(ButtonStartApp, command_button_clicked),

    ButtonRemoveNode = wxButton:new(Panel, ?wxID_ANY, [{label,"Remove Node"}]),
	wxButton:connect(ButtonRemoveNode, command_button_clicked),

	ButtonSendConfig = wxButton:new(Panel, ?wxID_ANY, [{label,"Send New Configurations"}]),
    wxButton:connect(ButtonSendConfig, command_button_clicked),
    ButtonFullMap = wxButton:new(Panel, ?wxID_ANY, [{label,"Show Full Map"}]),

    ButtonExport = wxButton:new(Panel, ?wxID_ANY, [{label,"Export Stats Now"}]),
    wxButton:connect(ButtonExport, command_button_clicked),


    wxButton:connect(ButtonFullMap, command_button_clicked),
    Canvas = wxPanel:new(Panel, [{size, {600,600}}]),

		UpdateLocation = wxToggleButton:new(Panel, ?wxID_ANY, "Selecet Location"),

    %% Radio Buttons:
    {RadioButtonSizer,ConfigButtons} = create_radio_buttons(Panel),

    %% Node Choice:
		NodeChoice = wxChoice:new(Panel, ?wxID_ANY, [{size,{150,40}}]),
    wxChoice:connect(NodeChoice, command_choice_selected),

    %% Add to sizers

    %% Management:
    wxSizer:add(ManagementSzLeftP, ButtonFullMap),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, NodeChoice),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, ButtonDeleteTable),
	wxSizer:addSpacer(ManagementSzLeftP, 10),
	wxSizer:add(ManagementSzLeftP, ButtonDeleteAll),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, CmbTo),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, TxtMsgSend),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, ButtonSendMSG),
    wxSizer:addSpacer(ManagementSzLeftP, 10),

    wxSizer:add(ManagementSzRightP, RadioButtonSizer),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzRightP, ButtonSendConfig),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzRightP, UpdateLocation),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzLeftP, ButtonExport),
    wxSizer:addSpacer(ManagementSzRightP, 30),
    wxSizer:add(ManagementSzRightP, NodesToFilterTxt),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzRightP, ButtonUpdateNodesToFilter),
	wxSizer:addSpacer(ManagementSzRightP, 30),
	wxSizer:add(ManagementSzRightP, ButtonStartApp),
	wxSizer:addSpacer(ManagementSzRightP, 30),
	wxSizer:add(ManagementSzRightP, ButtonRemoveNode),

    %% Nodes:
    wxSizer:add(NodesSz, Canvas),
    wxPanel:connect (Canvas, left_up),

    wxSizer:addSpacer(NodesSz, 10),


    %% LOGS:
    %Log = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, ""}, {size,{?X_SIZE,100}},
    %              {style, ?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    %wxSizer:add(LogSz, Log, []),
    Log = 1,
    wxSizer:addSpacer(LeftP, 10),
    wxSizer:add(LeftP, ManagementSz, []),
    wxSizer:add(LeftP, MikraPLC),
    wxSizer:add(LeftP, MikraRF),
    wxSizer:add(LeftP, Counters),

    wxSizer:add(OuterSz, LeftP, []),
    wxSizer:addSpacer(OuterSz, 20), % spacer
    wxSizer:add(OuterSz, NodesSz, []),

    wxSizer:add(TitleSz, Title, []),



    wxSizer:add(SuperSz, TitleSz, []),
        wxSizer:addSpacer(SuperSz, 10), % spacer
    wxSizer:add(SuperSz, OuterSz, []),
        wxSizer:addSpacer(SuperSz, 10), % spacer
    wxSizer:add(SuperSz, LogSz, []),


%% Now 'set' OuterSz into the Panel
    wxPanel:setSizer(Panel, SuperSz),

    wxFrame:show(Frame),
    erlang:send_after(?REFRESH_TIME,self(),timer),

    {Frame,#state{frame=Frame,panel=Panel,mapEts = MapEts, nodesEts = NodesEts, log = Log,canvas = Canvas,
                    cmbTo = CmbTo,txtMsgSend = TxtMsgSend, nodesToFilterList = NodesToFilterTxt, nodeChoice = NodeChoice, numberOfNodes = 0,
                    counters = Counters,
                    buttonExport = wxButton:getId(ButtonExport),
                    configButtons = ConfigButtons,
					buttonSendConfig = wxButton:getId(ButtonSendConfig),
                    buttonFullMap = wxButton:getId(ButtonFullMap),
                    updateLocation = UpdateLocation,
                    buttonDeleteTable = wxButton:getId(ButtonDeleteTable),
					buttonDeleteAll = wxButton:getId(ButtonDeleteAll),
					buttonUpdateNodesToFilter = wxButton:getId(ButtonUpdateNodesToFilter),
					buttonStartApp = wxButton:getId(ButtonStartApp),
					buttonRemoveNode = wxButton:getId(ButtonRemoveNode),
                    buttonSendMSG = wxButton:getId(ButtonSendMSG)}}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling sync event
%%
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event,_,State) ->
		%io:format("got _Event=~p in handle_sync_event~n",[_Event]),
		{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling WX events
%%
%%      event command_choice_selected:
%%            user selected a new node to represent on map and settings
%%              SelectedNode is updated to new node.
%%              Map is updated to show new node view.
%%
%%
%%      event command_button_clicked:
%%            user clicked a button
%%
%%              UpdateLocation button:
%%                  move SelectedNode to new location on the map
%%              ButtonFullMap button:
%%                  Show the all routes known.
%%              ButtonDeleteTable button:
%%                  Make SelectedNode reset itself.
%%              ButtonSendMSG button:
%%                  Make SelectedNode send a message.
%%              ButtonSendConfig button:
%%                  Update SelectedNode configurations.
%%              ButtonExport button:
%%                  Make stats_server export its data now.
%%
%% @end
%%--------------------------------------------------------------------

handle_event(#wx{event = #wxMouse{type = left_up, x = X, y = Y}},State = #state{nodesEts = NodesEts, updateLocation = UpdateLocation}) ->
		W = wxToggleButton:getValue(UpdateLocation),
		io:format("~n~nwxToggleButton:getValue(UpdateLocation) - ~p~n",[W]),

		case	wxToggleButton:getValue(UpdateLocation) of
			false ->
				io:format("~n~nPick up node from On: ~p,~p~n",[X,Y]),
				Node = find_node(State#state.nodesEts,ets:first(NodesEts), {X,Y}),
				case Node of
					ok -> {noreply,State};
					[{N,{_Time, _, _Mode,_RoutingSet, NodesToFilterList}}] ->
						wxChoice:setStringSelection(State#state.nodeChoice,atom_to_list(N)),
						update_map(State#state.canvas, N, State#state.nodesEts,State#state.mapEts,State#state.configButtons,State#state.updateLocation,State#state.nodeChoice, State#state.cmbTo),

						%TODO - Update nodes to filter per node
						wxTextCtrl:setValue(State#state.nodesToFilterList, NodesToFilterList),

						{noreply,State#state{selectedNode = N}}
				end;
			true ->
            		[{_,{Time, _, Mode,RoutingSet, NodesToFilterList}}] = ets:lookup(State#state.nodesEts,State#state.selectedNode),
		            io:format("~n~nUpdate location SelectedNode: ~p~n", [State#state.selectedNode]),
		            ets:insert(State#state.nodesEts,{State#state.selectedNode,{Time, {X,Y}, Mode,RoutingSet, NodesToFilterList}}),
		            %io:format("~n~nPlace node On: ~p,~p~n~n~n",[X,Y]),
					wxToggleButton:setValue(UpdateLocation,false),
					wxTextCtrl:setValue(State#state.nodesToFilterList, NodesToFilterList),

					{noreply,State}
			end;

handle_event(#wx{event=#wxCommand{type = command_choice_selected, cmdString=Ex}}, State) ->
    io:format("command_choice_selected ~p~n",[Ex]),
    SelectedNode = list_to_atom(Ex),
	update_map(State#state.canvas, SelectedNode, State#state.nodesEts,State#state.mapEts,State#state.configButtons,State#state.updateLocation,State#state.nodeChoice, State#state.cmbTo),
	[{_,{Time, _, _Mode,_RoutingSet, NodesToFilterList}}] = ets:lookup(State#state.nodesEts,SelectedNode),
	wxTextCtrl:setValue(State#state.nodesToFilterList, NodesToFilterList),

{noreply,State#state{selectedNode = SelectedNode}};

handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}},
	     State = #state{selectedNode = SelectedNode,buttonFullMap = ButtonFullMap,
                buttonDeleteTable = ButtonDeleteTable, buttonDeleteAll= ButtonDeleteAll, buttonSendConfig = ButtonSendConfig,
                buttonExport = ButtonExport, buttonSendMSG = ButtonSendMSG, buttonUpdateNodesToFilter = ButtonUpdateNodesToFilter,
				buttonStartApp = ButtonStartApp, buttonRemoveNode = ButtonRemoveNode}) ->
    case ID of
        ButtonFullMap ->
            	io:format("Showing full map~n"),
							wxChoice:setSelection(State#state.nodeChoice,-1), %%%%%%%%%%%%%%%%%%%%%%%
				update_map(State#state.canvas, all, State#state.nodesEts,State#state.mapEts,State#state.configButtons,State#state.updateLocation,State#state.nodeChoice, State#state.cmbTo),
            {noreply,State#state{selectedNode = all}};
        ButtonDeleteTable->
                    io:format("buttonDeleteTable need to delete ~p~n",[SelectedNode]),
                    node_control_interface:reset_node(SelectedNode),
                    %io:format("buttonDeleteTable delete ~p res ~p~n",[SelectedNode, A]),
                    {noreply,State};
		ButtonDeleteAll ->
                    io:format("ButtonDeleteAll need to delete everything~n"),
										node_control_interface:routing_tables_cleared_from_gui(),
					resetAllNodes(State#state.nodesEts, ets:first(State#state.nodesEts)),
                    %io:format("buttonDeleteTable delete ~p res ~p~n",[SelectedNode, A]),
                    {noreply,State};
		ButtonStartApp ->
                    io:format("ButtonStartApp starting application on all nodes~n"),
					start_application(State#state.nodesEts, ets:first(State#state.nodesEts)),
                    {noreply,State};
		ButtonRemoveNode ->
                    io:format("ButtonRemoveNode : removing node ~p~n", [SelectedNode]),
					node_control_interface:disable(SelectedNode),
                    {noreply,State};
        ButtonSendMSG ->
                    io:format("buttonSendMSG send: ~p~n TO: ~p~n",[wxTextCtrl:getValue(State#state.txtMsgSend), wxChoice:getStringSelection(State#state.cmbTo)]),
                    node_control_interface:initiate_transaction(SelectedNode, list_to_atom(wxChoice:getStringSelection(State#state.cmbTo)), wxTextCtrl:getValue(State#state.txtMsgSend)),
				                    % wxTextCtrl:clear(State#state.txtMsgSend),
                    {noreply,State};
        ButtonUpdateNodesToFilter ->
                    io:format("ButtonUpdateNodesToFilter send: ~p~n Nodes: ~p~n",[wxTextCtrl:getValue(State#state.nodesToFilterList), wxChoice:getStringSelection(State#state.cmbTo)]),
					NodesToFilterList = getNodesToFilterList(wxTextCtrl:getValue(State#state.nodesToFilterList)),
					node_control_interface:update_nodes_to_filter(SelectedNode, NodesToFilterList),
                    {noreply,State};

        ButtonSendConfig ->
                    %node_control_interface:update_configuration(SelectedNode, checkRadio(State#state.configButtons)),
										RadioState = checkRadio(State#state.configButtons),
										node_control_interface:configuration_updated_from_gui ([SelectedNode, RadioState]),
                    io:format("~n~n~n AAAA ~p ~n~n~n~n",[checkRadio(State#state.configButtons)]),
                    node_control_interface:update_configuration(SelectedNode,RadioState),
					node_control_interface:reset_node(SelectedNode),
                    %rpc:cast(SelectedNode, node_control_interface, update_configuration, [SelectedNode, checkRadio(State#state.configButtons)]),
                    io:format("ButtonSendConfig~n"),
                    {noreply,State};
        ButtonExport ->
                    io:format("ButtonExport clicked~n"),
                    stats_server:export(),
                    {noreply,State};
        A ->
                io:format("unknown ID handle_event: ~p~n", [A]),
                {noreply,State}
    end;


handle_event(_, State) -> io:format("graphic handle_event nothing interesting~n"),
{noreply,State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%      {node_state,Data}:
%%          node new state, Data contains name of node, routing set of node and medium mode.
%%%%Data:{node_state,[{node_name,node_24},{routing_set,[{{destination,0},{next_address,0},{medium,3}}]},{medium_mode,dual}]}
%% @end
%%--------------------------------------------------------------------
handle_cast({node_state,Data}, State = #state{nodeChoice = NodeChoice, cmbTo = CmbTo}) ->

    NodeNameAtom = proplists:get_value(node_name, Data),
    NodeNameList = atom_to_list(NodeNameAtom),
    RoutingSet = proplists:get_value(routing_set, Data),

    case ets:member(State#state.nodesEts, NodeNameAtom) of
        false ->
            io:format("loadNGgui.erl: new_node_is_up: ID=~p~n",[NodeNameList]),
            wxChoice:append(NodeChoice,NodeNameList),
            wxChoice:append(CmbTo, NodeNameList),
            Location = {rand:uniform(500),rand:uniform(500)};
            %Location = findLocation(NodeNameAtom);
        true ->
            %io:format("loadNGgui.erl: node updated: ID=~p~n",[NodeNameAtom]),
            [{NodeNameAtom,{_,Location, _,_,_}}] = ets:lookup(State#state.nodesEts,NodeNameAtom)
    end,
		Time = get_current_millis(),
		NodeInfo = {NodeNameAtom, {Time, Location, proplists:get_value(medium_mode, Data),RoutingSet, list_of_integers_to_string(proplists:get_value(nodes_to_filter, Data))}},
		% io:format("Data:  ~p~n",[Data]),
		% io:format("NodeInfo:  ~p~n",[NodeInfo]),

    ets:insert(State#state.nodesEts, NodeInfo),
    update_map_ets(State#state.mapEts,NodeNameAtom,RoutingSet),
    %printRoutingSet(State#state.selectedNode, NodeNameAtom, State#state.nodesEts),
    {noreply, State};

handle_cast({printNodes}, State) ->
    io:format("loadNGgui.erl: print nodesEts:~n"),
    printNodesEts(ets:first(State#state.nodesEts), State#state.nodesEts),
    {noreply, State};

handle_cast({node_is_down,DownNode}, State) ->
    io:format("loadNGgui.erl: Node down: ~p~n",DownNode),
    %% TODO: remove node from list.
    {noreply, State};

	handle_cast({resetAllNodes}, State) ->
		io:format("loadNGgui.erl: resetAllNodes~n"),
		resetAllNodes(State#state.nodesEts, ets:first(State#state.nodesEts)),
		{noreply, State};

	handle_cast({remove_stations, StationList}, State) ->
		remove_stations(State#state.nodesEts, StationList),
		io:format("loadNGgui.erl: remove_stations ~p~n",[StationList]),
		{noreply, State};

		handle_cast({update_medium, ListOfNodesAndMediums}, State) ->
			update_medium(State#state.nodesEts, ListOfNodesAndMediums),
			io:format("loadNGgui.erl: update_medium ~p~n",[ListOfNodesAndMediums]),
			{noreply, State};

handle_cast(A, State) ->
	io:format("~n~nloadNGgui.erl: unhandled cast msg=~p~n~n",[A]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%%--------------------------------------------------------------------
handle_info(timer, State) ->
 % io:format("loadNG: timer~n"),
  stats_server:stats_request(self()),
	update_map(State#state.canvas, State#state.selectedNode, State#state.nodesEts,State#state.mapEts,ok,ok,State#state.nodeChoice, State#state.cmbTo),

  erlang:send_after(?REFRESH_TIME,self(),timer),
  {noreply, State};

handle_info({update_metrics, Counters}, State) ->
  % io:format("loadNG: Got State Update~p, ~p, ~p~n",[Counters, AvgTime,AvgLength]),
  wxStaticText:setLabel(State#state.counters,
                "Number Of ManagementMsgSent = " ++ integer_to_list(Counters#counters.numberOfManagementMsgSent) ++
                "\nNumber Of ManagementMsgReceived = "++ integer_to_list(Counters#counters.numberOfManagementMsgReceived) ++
                "\nNumber Of DataMsgSent = "++ integer_to_list(Counters#counters.numberOfDataMsgSent) ++
                "\nNumber Of DataMsgReceived = "++ integer_to_list(Counters#counters.numberOfDataMsgReceived) ++
                "\nNumber Of RelayMsg = "++ integer_to_list(Counters#counters.numberOfRelayMsg) ++
                "\nEnd to End Data Message Average Delay: " ++ float_to_list(Counters#counters.data_msg_avg_time)  ++
				"\nAverage Data Message Route Length: " ++ float_to_list(0.0)),
  {noreply, State};

handle_info(E, State) ->
	io:format("handle_info _Event ~p~n",[E]),
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%%--------------------------------------------------------------------
handle_call(shutdown, _From, State=#state{panel=Panel}) ->
    wxPanel:destroy(Panel),
    stats_server:stop(),
    {stop, normal, ok, State};


handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling terminate
%%
%%--------------------------------------------------------------------
terminate(_Reason, #state{frame=Frame}) ->
  io:format("terminate, reason=~p ",[_Reason]),
  stats_server:stop(),

  wxWindow:destroy(Frame),
    ok.

code_change(_, _, State) ->
	io:format("code_change ~n"),
	{noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%    DEBUG:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
printNodesEts('$end_of_table', _Ets) ->
    ok;
printNodesEts(Node, Ets) ->
    io:format("Node: ~p~n",[ets:lookup(Ets,Node)]),

    printNodesEts(ets:next(Ets, Node), Ets).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Internal Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_stations(_,[]) ->
	ok;
remove_stations(NodesEts,[Station|StationList]) ->
	io:format("Assuming it should turn off Node- ~p~n",[Station]),
	node_control_interface:update_configuration(Station, idle),
	node_control_interface:reset_node(Station),
	remove_stations(NodesEts,StationList).

update_medium(_,[]) ->
	ok;
update_medium(NodesEts,[{NodeName, MediumType}|ListOfNodesAndMediums]) ->
	io:format("Assuming it should change Node- ~p to ~p~n",[NodeName, MediumType]),
	node_control_interface:update_configuration(NodeName, MediumType),
	update_medium(NodesEts,ListOfNodesAndMediums).

update_map_ets(MapEts, Node, NodeRoutingMap) ->
			ets:match_delete(MapEts,{{Node, '_'},'_'}),
			update_map_ets_2(MapEts, Node, NodeRoutingMap).

update_map_ets_2(_, _, [])-> ok;
update_map_ets_2(MapEts, Node, [{{destination,0},{next_address,0},_}|NodeRoutingMap])->
    update_map_ets_2(MapEts, Node, NodeRoutingMap);
update_map_ets_2(MapEts, Node, [{{destination,NextNode},{next_address,NextNode},{medium,Medium}}|NodeRoutingMap])->
    NextNodeAtom = makeAtom(NextNode), %  DestNodeAtom = makeAtom(DestNode),ets:insert(MapEts,{{Node, {NextNodeAtom, DestNodeAtom}},{0}}),
    ets:insert(MapEts,{{Node, NextNodeAtom},{Medium}}),
    update_map_ets_2(MapEts, Node, NodeRoutingMap);

update_map_ets_2(MapEts, Node, [_|NodeRoutingMap])->
	update_map_ets_2(MapEts, Node, NodeRoutingMap);

update_map_ets_2(A, B, C)->
	io:format("IGNORING update_map_ets_2 - received Values: A: ~p, B: ~p, C: ~p", [A, B, C]),
	ok.


find_node(_,'$end_of_table',_) -> ok;
find_node(NodesEts,Key, MouseLocation) ->
		Node = ets:lookup(NodesEts,Key),
		[{Key,{_, KeyLocation,_, _, _}}] = Node,
		case distance(KeyLocation, MouseLocation) of
				N when N =< ?CIRCE_RADIUS_SQURE ->
					Node;
				_ ->
					find_node(NodesEts,ets:next(NodesEts,Key), MouseLocation)
				end.

distance({X1,Y1}, {X2,Y2}) ->
    math:pow(X1-X2, 2) + math:pow(Y1-Y2, 2).


%update_map(Canvas, all, NodesEts,ConfigButtons,UpdateLocation) -> ok;
update_map(Canvas, SelectedNode, NodesEts,MapEts,ConfigButtons,UpdateLocation,NodeChoice, CmbTo) ->
		DC = wxWindowDC:new(Canvas),
		wxDC:clear(DC),
		case SelectedNode of
			all -> 	switch_to_full_map(DC, ets:first(MapEts), MapEts, NodesEts);
			_ ->
				Result = ets:lookup(NodesEts,SelectedNode),
				case Result of
					[{SelectedNode,{_, {X,Y}, MediumMode, RoutingSet, NodesToFilter}}] ->
							%draw_routes_from_node(DC, SelectedNode, {X,Y},NodesEts,RoutingSet),
              io:format("draw map from SelectedNode ~p, RoutingSet ~p~n",[SelectedNode, RoutingSet]),

      				draw_routes_from_node_to_each_node(DC, NodesEts, SelectedNode, RoutingSet),
							configButtonUpdate(MediumMode ,ConfigButtons);
				     [] -> ok
			end
		end,

		draw_nodes(DC, SelectedNode ,ets:tab2list(NodesEts), NodesEts,NodeChoice, CmbTo),
		if UpdateLocation =/= ok -> wxToggleButton:setValue(UpdateLocation,false); true -> ok end,
		wxWindowDC:destroy(DC).

	%%%%
	%%  Draws all nodes
	%%%%
draw_nodes(_, _, [] , _,_,_) -> ok;
draw_nodes(DC, SelectedNode, [Head|RestNodes], NodesEts,NodeChoice, CmbTo) ->
		% Result = ets:lookup(NodesEts,NodeKey),
		case Head of
			{NodeKey,{OldTime, {X,Y}, _MediumMode, _, _}} ->
				Time = get_current_millis(),
				if OldTime == -1 -> ok; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
					Time - OldTime < ?TIMEOUT ->
						wxDC:drawCircle(DC, {X,Y}, ?CIRCE_RADIUS),
						wxDC:drawLabel(DC,atom_to_list(NodeKey), {X-10,Y-10,X+50,Y+50});
					Time - OldTime >= ?TIMEOUT ->
						io:format("TIMEOUT ~p~n",[NodeKey]),
						ets:delete(NodesEts, NodeKey), %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						wxChoice:delete(NodeChoice, wxChoice:findString(NodeChoice, atom_to_list(NodeKey))),
						wxChoice:delete(CmbTo, wxChoice:findString(CmbTo, atom_to_list(NodeKey)))
					end;
		_ ->
			io:format("draw_nodes UNEXPECTED ERROR:  ~p~n",[Head])
		end,
		draw_nodes(DC, SelectedNode, RestNodes, NodesEts,NodeChoice, CmbTo).


%%%%
%%  Draws The full map of all the nodes
%%%%
switch_to_full_map(_, '$end_of_table', _, _) -> ok;
switch_to_full_map(DC, MapEtsKey, MapEts, NodesEts) ->
    [{{Node,NextNode},{Medium}}] = ets:lookup(MapEts,MapEtsKey),
    io:format("Node: ~p NextNode: ~p~n",[Node,NextNode]),
		case {ets:lookup(NodesEts,Node), ets:lookup(NodesEts,NextNode)} of
			{[{Node,{_, NodeLocation,_, _,_}}], [{NextNode,{_, NextNodeLocation,_, _,_}}]} ->
				draw_route(DC, NodeLocation,NextNodeLocation, Medium);
				A -> io:format("switch_to_full_map: Node ~p or NextNode ~p are not in ETS ~p~n",[Node, NextNode, A]), ok
		end,
    switch_to_full_map(DC, ets:next(MapEts,MapEtsKey), MapEts, NodesEts).

%%%%
%%  Draws all routes the SelectedNode familiar
%%%%
draw_routes_from_node(_, _, _,_,[]) ->     %io:format("draw_routes_from_node ends~n"),
ok;
draw_routes_from_node(DC, SelectedNode, Location, NodesEts,[{{destination, 0},_, _}|RoutingSet]) ->
	draw_routes_from_node(DC, SelectedNode, Location, NodesEts,RoutingSet);
draw_routes_from_node(DC, SelectedNode, Location, NodesEts,[{{destination, Node}, {next_address, Node}, {medium, Medium}}|RoutingSet]) ->
    AtomNode = makeAtom(Node),
    %io:format("draw_routes_from_node N: ~p~n~n",[AtomNode]),

	case ets:lookup(NodesEts,AtomNode) of
		[{AtomNode,{_, NextLocation, _,_, _}}] ->
			draw_route(DC, Location,NextLocation, Medium);
		[] -> %io:format("draw_routes_from_node not found node"),
			ok
	end,
	draw_routes_from_node(DC, SelectedNode, Location, NodesEts,RoutingSet);


draw_routes_from_node(DC, SelectedNode, Location, NodesEts,[{{destination, Node1}, {next_address, Node2}, {medium, Medium}}|RoutingSet]) ->
	AtomNode1 = makeAtom(Node1),
	%io:format("draw_routes_from_node N: ~p~n~n",[AtomNode1]),
	AtomNode2 = makeAtom(Node2),
		case { ets:lookup(NodesEts,AtomNode1),ets:lookup(NodesEts,AtomNode2)}of
			{[{AtomNode1,{_, Location1, _,_,_}}], [{AtomNode2,{_, Location2,_,_,_}}]} ->

			    draw_route(DC, Location,Location2, Medium),
			    draw_route(DC, Location1,Location2, 0);

			_ -> %io:format("draw_routes_from_node not found node"),
				ok
		end,
    draw_routes_from_node(DC, SelectedNode, Location, NodesEts,RoutingSet);

draw_routes_from_node(_, _, _,_,_) -> ok.

draw_routes_from_node_to_each_node(DC, NodesEts, SourceNode, [])-> ok;
draw_routes_from_node_to_each_node(DC, NodesEts, SourceNode, [{{destination, DestinationNode},_, _}|RoutingSet])->
	draw_routes_from_node_to_node(DC, SourceNode, DestinationNode, NodesEts),
	draw_routes_from_node_to_each_node(DC, NodesEts, SourceNode, RoutingSet).

draw_routes_from_node_to_node(DC, SourceNode, SourceNode, NodesEts) -> ok;
draw_routes_from_node_to_node(DC, SourceNode, DestinationNode, NodesEts) ->
	io:format("draw_routes_from_node_to_node SourceNode ~p, DestinationNode ~p~n",[SourceNode, DestinationNode]),
	[{SourceNode,{_, Location1, _,RoutingSet, _}}] = ets:lookup(NodesEts,SourceNode),
	case findNodeInRoutingSet2(DestinationNode, RoutingSet) of
		{{destination, DestinationNode}, {next_address, Node}, {medium, Medium}} ->
			NextNode = makeAtom(Node),
			[{NextNode,{_, Location2, _,_, _}}] = ets:lookup(NodesEts,NextNode),
			draw_route(DC, Location1,Location2, Medium),
			draw_routes_from_node_to_node(DC, NextNode, DestinationNode, NodesEts);
		_ ->	io:format("No Route to DestinationNode ~p from ~p RoutingSet: ~p~n",[DestinationNode, SourceNode,RoutingSet]),
				ok
	end.

findNodeInRoutingSet(DestinationNode, [{{destination, DestinationNode}, {next_address, Node}, {medium, Medium}} | RoutingSet]) ->
	{makeAtom(Node), Medium};
findNodeInRoutingSet(DestinationNode, [{{destination, Node1}, {_, _}}|RoutingSet]) ->
	findNodeInRoutingSet(DestinationNode,RoutingSet);
findNodeInRoutingSet(DestinationNode, []) ->
	ok.

findNodeInRoutingSet2(DestinationNode,RoutingSet)->
	lists:keyfind({destination, DestinationNode}, 1, RoutingSet).

%%%%
%%  Draws a line (representing a communication line) from Location1 to Location2 according to Medium
%%%%
draw_route(DC, Location1,Location2, Medium) ->
    %io:format("Medium: ~p~n",[Medium]),
    case Medium of
        2 -> %PLC
            Colour = ?wxBLUE;
        1 -> %RF
            Colour = ?wxRED;
        _ ->
            Colour = ?wxBLACK
    end,
    wxDC:setPen(DC, wxPen:new(Colour)),
    wxDC:drawLine(DC, Location1, Location2),
    wxDC:setPen(DC, wxPen:new(?wxBLACK)).

makeAtom(NodeNumber) -> list_to_atom("node_" ++ integer_to_list(NodeNumber)).

configButtonUpdate(_, ok)->ok;
configButtonUpdate(idle, [PlcOff,_,RfOff,_])->selectRadio(PlcOff,RfOff);
configButtonUpdate(rf_only, [PlcOff,_,_,RfOn])->selectRadio(PlcOff,RfOn);
configButtonUpdate(plc_only, [_,PlcOn,RfOff,_])->selectRadio(PlcOn,RfOff);
configButtonUpdate(dual, [_,PlcOn,_,RfOn])->selectRadio(PlcOn,RfOn);
configButtonUpdate(undefined, _)-> io:format("Medium mode is UNDEFINED, skiping"), ok.


selectRadio(Plc,Rf) -> wxRadioButton:setValue(Plc,true), wxRadioButton:setValue(Rf,true).

checkRadio(false,false) -> idle;
checkRadio(false,true) -> rf_only;
checkRadio(true,false) -> plc_only;
checkRadio(true,true) -> dual;
checkRadio(Plc,Rf) -> {wxRadioButton:getValue(Plc), wxRadioButton:getValue(Rf)}.

checkRadio([_,PLC,_,RF]) ->checkRadio(wxRadioButton:getValue(PLC), wxRadioButton:getValue(RF)).


resetAllNodes(_, '$end_of_table') -> ok;
resetAllNodes(NodesEts, Key) ->
	node_control_interface:reset_node(Key),
	resetAllNodes(NodesEts, ets:next(NodesEts,Key)).


start_application(_, '$end_of_table') -> ok;
start_application(NodesEts, Key) ->
	node_control_interface:start_application(Key),
	start_application(NodesEts, ets:next(NodesEts,Key)).

printRoutingSet(SelectedNode, SelectedNode, NodesEts) ->
	A = ets:lookup(NodesEts,SelectedNode),
	io:format("~n~nSelectedNode Info: ~n~p~n~n",[A]);
printRoutingSet(_SelectedNode, _NameAtom, _NodesEts) -> ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Create Form Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_radio_buttons(Panel) ->
    RadioButtonSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "wxRadioButton"}]),
    Buttons =
        [wxRadioButton:new(Panel, ?wxID_ANY, "PLC OFF",[{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "PLC ON", []),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF OFF",[{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF ON", [])],

    Fun =fun(Item) ->
            wxSizer:add(RadioButtonSizer, Item)
        end,
    wx:foreach(Fun, Buttons),
    {RadioButtonSizer,Buttons}.


getNodesToFilterList(NodesToFilterString)->
	case NodesToFilterString of
		"Accept All" ->	[];
		" " ->	[];
		"" ->	[];
		[] -> [];
		_ ->
			[ list_to_integer(X) || X <- trim_string(NodesToFilterString)]
			% []
	end.

trim_string(String)->
    trim_string(String, 32, "", []).

trim_string([], _Delimiter, ValueAcc, Acc)-> Acc ++ [ValueAcc];
trim_string([H|RestString], Delimiter, ValueAcc, Acc)->
    % io:format("[~p]: trim_string H: ~p, RestString: ~p, Delimiter: ~p, ValueAcc: ~p, Acc: ~p~n", [?MODULE, H, RestString, Delimiter, ValueAcc, Acc]),
    case H of
        Delimiter ->
            trim_string(RestString, Delimiter, "", Acc ++ [ValueAcc]);
        _ ->
            trim_string(RestString, Delimiter, ValueAcc ++ [H], Acc)
    end.


list_of_integers_to_string([]) -> [];
list_of_integers_to_string(ListOfIntegers) when is_list(ListOfIntegers) -> [ integer_to_list(X) ++ " " || X <- ListOfIntegers];
list_of_integers_to_string(_) -> [].

get_current_millis() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).
