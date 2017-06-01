-module(loadNGgui).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(SERVER, ?MODULE).
-define(X_SIZE, 1080).
-define(Y_SIZE, 680).
-define(REFRESH_TIME, 1000).
-define( LOG_DIR,"./logger/").


-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2,handle_cast/2, handle_call/3, handle_event/2, handle_sync_event/3]).

-record(state,
	{frame,panel, mapEts, nodesEts, canvas, dc, log, nodeChoice, selectedNode = all, numberOfNodes, locationInput, buttons,%}).
%-record(buttons, {
    counters,
    configButtons, updateLocation,
    buttonExport, buttonFullMap, buttonDeleteTable,buttonSendConfig, buttonSendMSG, txtMsgSend, cmbTo}).

-record(routing_set_entry, {dest_addr, next_addr, medium}).

-record(counters, {numberOfRelayMsg, numberOfManagementMsgSent, numberOfManagementMsgReceived, numberOfDataMsgSent, numberOfDataMsgReceived}).






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

    Frame = wxFrame:new(WxServer, ?wxID_ANY, "LOADng", [{size,{?X_SIZE, ?Y_SIZE+40}}]),
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

    %NodesSz = wxBoxSizer:new(?wxVERTICAL),
    %ManagementSz = wxBoxSizer:new(?wxVERTICAL),



    %% create widgets
    MikraPLC = wxStaticText:new(Panel, ?wxID_ANY," PLC |",[{style, ? wxALIGN_RIGHT}]),
    MikraRF = wxStaticText:new(Panel, ?wxID_ANY," RF   |",[{style, ? wxALIGN_RIGHT}]),
    Counters = wxStaticText:new(Panel, ?wxID_ANY,"Stats Server is loading",[{style, ? wxALIGN_LEFT}]),

    wxStaticText:setForegroundColour(MikraPLC, ?wxBLUE),
    wxStaticText:setForegroundColour(MikraRF, ?wxRED),
    Title = wxStaticText:new(Panel, ?wxID_ANY,"Smart Meter Network Management Tool:",[{style, ?wxALIGN_CENTER}]),


    %%Setup Buttons:
    ButtonDeleteTable = wxButton:new(Panel, ?wxID_ANY, [{label,"Delete Routes Table"}]),
    wxButton:connect(ButtonDeleteTable, command_button_clicked),

    CmbTo = wxComboBox:new(Panel, ?wxID_ANY, [{choices, []},{style, ?wxCB_READONLY}]),
    TxtMsgSend = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, ""}, {size,{170,130}},
                  {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),

    ButtonSendMSG = wxButton:new(Panel, ?wxID_ANY, [{label,"Send Message"}]),
    wxButton:connect(ButtonSendMSG, command_button_clicked),

    ButtonSendConfig = wxButton:new(Panel, ?wxID_ANY, [{label,"Send New Configurations"}]),
    wxButton:connect(ButtonSendConfig, command_button_clicked),
    ButtonFullMap = wxButton:new(Panel, ?wxID_ANY, [{label,"Show Full Map"}]),

    ButtonExport = wxButton:new(Panel, ?wxID_ANY, [{label,"Export Stats Now"}]),
    wxButton:connect(ButtonExport, command_button_clicked),


    wxButton:connect(ButtonFullMap, command_button_clicked),
    Canvas = wxPanel:new(Panel, [{size, {600,600}}]),

    %%Text input:
    XNodeLocation  = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "X location"},{style, ?wxDEFAULT}]),
    YNodeLocation  = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "Y location"},{style, ?wxDEFAULT}]),
    UpdateLocation = wxButton:new(Panel, ?wxID_ANY, [{label,"Update Location"}]),
    wxButton:connect(UpdateLocation, command_button_clicked),

    %% Radio Buttons:
    {RadioButtonSizer,ConfigButtons} = create_radio_buttons(Panel),

    %% Node Choice:
    NodeChoice = wxListBox:new(Panel, ?wxID_ANY, [{size,{150,100}},{style, ?wxLB_SINGLE}]),
    wxListBox:connect(NodeChoice, command_listbox_selected),

    %% Add to sizers

    %% Management:
    wxSizer:add(ManagementSzLeftP, ButtonFullMap),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, NodeChoice),
    wxSizer:addSpacer(ManagementSzLeftP, 10),
    wxSizer:add(ManagementSzLeftP, ButtonDeleteTable),
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
    wxSizer:add(ManagementSzRightP, XNodeLocation),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzRightP, YNodeLocation),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzRightP, UpdateLocation),
    wxSizer:addSpacer(ManagementSzRightP, 10),
    wxSizer:add(ManagementSzLeftP, ButtonExport),
    wxSizer:addSpacer(ManagementSzLeftP, 10),

    %% Nodes:
    wxSizer:add(NodesSz, Canvas),

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



    wxSizer:add(SuperSz, TitleSz, []),`
        wxSizer:addSpacer(SuperSz, 10), % spacer
    wxSizer:add(SuperSz, OuterSz, []),
        wxSizer:addSpacer(SuperSz, 10), % spacer
    wxSizer:add(SuperSz, LogSz, []),


%% Now 'set' OuterSz into the Panel
    wxPanel:setSizer(Panel, SuperSz),


    %% TEST let DC be always alive
    DC = wxWindowDC:new(Canvas),

    wxFrame:show(Frame),
    erlang:send_after(?REFRESH_TIME,self(),timer),

    {Frame,#state{frame=Frame,panel=Panel,mapEts = MapEts, nodesEts = NodesEts, log = Log,canvas = Canvas,
                    dc = DC, cmbTo = CmbTo,txtMsgSend = TxtMsgSend, nodeChoice = NodeChoice, numberOfNodes = 0,
                    locationInput =  {XNodeLocation, YNodeLocation}, buttonSendConfig = ButtonSendConfig,
                    %buttons = #buttons{
                    counters = Counters,
                    buttonExport = wxButton:getId(ButtonExport),
                                        configButtons = ConfigButtons,
                                        buttonFullMap = wxButton:getId(ButtonFullMap),
                                        updateLocation = wxButton:getId(UpdateLocation),
                                        buttonDeleteTable = wxButton:getId(ButtonDeleteTable),
                                        buttonSendMSG = wxButton:getId(ButtonSendMSG)
                                        %}
                    }}.


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
%%      event command_listbox_selected:
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

handle_event(#wx{event=#wxCommand{type = command_listbox_selected, cmdString=Ex}}, State = #state{nodesEts = NodesEts, configButtons = ConfigButtons}) ->
    io:format("command_listbox_selected ~p~n",[Ex]),
    SelectedNode = list_to_atom(Ex),
    DC = wxWindowDC:new(State#state.canvas),
    wxDC:clear(DC),

    switch_to_node(DC, SelectedNode,ets:first(NodesEts), NodesEts, State#state.locationInput),
    [{SelectedNode,{_, MediumMode,_}}] = ets:lookup(NodesEts,SelectedNode),
    configButtonUpdate(MediumMode,ConfigButtons),
    wxWindowDC:destroy(DC),

  %  wxTextCtrl:appendText(Log, "Selected node ID: " ++ NodeName ++"\n"),

{noreply,State#state{selectedNode = SelectedNode}};

handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}},
	     State = #state{selectedNode = SelectedNode,buttonFullMap = ButtonFullMap, updateLocation = UpdateLocation, locationInput =  {XNodeLocation, YNodeLocation},
                buttonDeleteTable = ButtonDeleteTable,
                buttonSendConfig = ButtonSendConfig,
                buttonExport = ButtonExport,
                buttonSendMSG = ButtonSendMSG
         }) ->


    case ID of
        UpdateLocation ->
            io:format("Update location clicked Selected: SelectedNode: ~p~n", [SelectedNode]),

            [{NodeNameAtom,{_, Mode,RoutingSet}}] = ets:lookup(State#state.nodesEts,SelectedNode),

            X = list_to_integer(wxTextCtrl:getLineText(XNodeLocation,0)),
            Y = list_to_integer(wxTextCtrl:getLineText(YNodeLocation,0)),


            io:format("Update location to ~p,~p SelectedNode: ~p~n", [X,Y, SelectedNode]),
            ets:insert(State#state.nodesEts,{NodeNameAtom,{{X,Y}, Mode,RoutingSet}}),
            {noreply,State};
        ButtonFullMap ->
            io:format("Showing full map~n"),
            DC = wxWindowDC:new(State#state.canvas),
            wxDC:clear(DC),
            draw_circle(DC,ets:first(State#state.nodesEts), State#state.nodesEts),

            switch_to_full_map(DC, ets:first(State#state.mapEts), State#state.mapEts, State#state.nodesEts),
            {_,L} = wxListBox:getSelections(State#state.nodeChoice),
            lists:map(fun (X) -> wxListBox:deselect(State#state.nodeChoice,X) end,L),
            wxWindowDC:destroy(DC),
            {noreply,State#state{selectedNode = all}};

        ButtonDeleteTable->
                    io:format("buttonDeleteTable need to delete ~p~n",[SelectedNode]),

                    %rpc:cast(SelectedNode, node_control_interface, reset_node, [SelectedNode]),
                    node_control_interface:reset_node(SelectedNode),
                    
                    {noreply,State};
                    %{initiate_transaction, [{destination, Destination}]]
        ButtonSendMSG ->
                    io:format("buttonSendMSG send: ~p~n TO: ~p~n",[wxTextCtrl:getValue(State#state.txtMsgSend), wxComboBox:getValue(State#state.cmbTo)]),

                    %rpc:cast(SelectedNode, stubNode, initiate_transaction, [SelectedNode, wxComboBox:getValue(State#state.cmbTo), wxTextCtrl:getValue(State#state.txtMsgSend)]),
                    node_control_interface:initiate_transaction(SelectedNode, wxComboBox:getValue(State#state.cmbTo), wxTextCtrl:getValue(State#state.txtMsgSend)),
                    wxTextCtrl:clear(State#state.txtMsgSend),
                    {noreply,State};
        ButtonSendConfig ->
                    node_control_interface:update_configuration(SelectedNode, checkRadio(State#state.configButtons)),
                    %rpc:cast(SelectedNode, stubNode, update_configuration, [SelectedNode, {P,R}]),
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
%%      
%% @end
%%--------------------------------------------------------------------
handle_cast({node_state,Data}, State = #state{nodeChoice = NodeChoice, cmbTo = CmbTo}) ->

    %%Data:{node_state,[{node_name,node_24},{routing_set,[{{destination,0},{next_address,0},{medium,3}}]},{medium_mode,dual}]}
  %  io:format ("loadNggui: node_state Data - ~p~n",[Data]),
  %  io:format ("loadNggui: node_state Data~n"),

    NodeNameAtom = proplists:get_value(node_name, Data),
    NodeNameList = atom_to_list(NodeNameAtom),

	RoutingSet = proplists:get_value(routing_set, Data),

    case ets:member(State#state.nodesEts, NodeNameAtom) of
        false ->
            io:format("loadNGgui.erl: new_node_is_up: ID=~p~n",[NodeNameList]),
            wxListBox:insertItems(NodeChoice,[NodeNameList],0),
            wxComboBox:append(CmbTo, NodeNameList),
            Location = {rand:uniform(500),rand:uniform(500)};
        true -> 
            io:format("loadNGgui.erl: node updated: ID=~p~n",[NodeNameAtom]),
            [{NodeNameAtom,{Location, _,_}}] = ets:lookup(State#state.nodesEts,NodeNameAtom)
    end,
    ets:insert(State#state.nodesEts,{NodeNameAtom,{Location, proplists:get_value(medium_mode, Data),RoutingSet}}),
    update_map_ets(State#state.mapEts,NodeNameAtom,RoutingSet),
    {noreply, State};

handle_cast({printNodes}, State) ->
    io:format("loadNGgui.erl: print nodesEts:~n"),
    printNodesEts(ets:first(State#state.nodesEts), State#state.nodesEts),
    {noreply, State};

handle_cast({node_is_down,DownNode}, State) ->
    io:format("loadNGgui.erl: Node down: ~p~n",DownNode),
    %% TODO: remove node from list.
    {noreply, State};

handle_cast(_A, State) ->
	io:format("~n~nloadNGgui.erl: unhandled cast msg=~p~n~n",[_A]),
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
  erlang:send_after(?REFRESH_TIME,self(),timer),
  {noreply, State};

handle_info({Counters = #counters{}, Avg}, State) ->
  io:format("loadNG: Got State Update~p~n",[Counters]),
  wxStaticText:setLabel(State#state.counters,
                "numberOfManagementMsgSent = " ++ integer_to_list(Counters#counters.numberOfManagementMsgSent) ++
                "\nnumberOfManagementMsgReceived = "++ integer_to_list(Counters#counters.numberOfManagementMsgReceived) ++
                "\nnumberOfDataMsgSent = "++ integer_to_list(Counters#counters.numberOfDataMsgSent) ++
                "\nnumberOfDataMsgReceived = "++ integer_to_list(Counters#counters.numberOfDataMsgReceived) ++
                "\nAverage time: " ++ float_to_list(Avg)
                ),
  {noreply, State};
handle_info(_, State) ->
	io:format("handle_info _Event~n"),
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
update_map_ets(_, _, [])-> ok;
update_map_ets(MapEts, Node, [{{destination,0},{next_address,0},{medium,_}}|NodeRoutingMap])->
    update_map_ets(MapEts, Node, NodeRoutingMap);
update_map_ets(MapEts, Node, [{{destination,DestNode},{next_address,NextNode},{medium,Medium}}|NodeRoutingMap])->
    NextNodeAtom = makeAtom(NextNode),
    DestNodeAtom = makeAtom(DestNode),
    ets:insert(MapEts,{{Node, NextNodeAtom},{Medium}}),
  %  ets:insert(MapEts,{{Node, {NextNodeAtom, DestNodeAtom}},{0}}),
    update_map_ets(MapEts, Node, NodeRoutingMap).


%%%%
%%  Draws The full map of all the nodes
%%%%
switch_to_full_map(_, '$end_of_table', _, _) -> ok;
switch_to_full_map(DC, MapEtsKey, MapEts, NodesEts) ->
    [{{Node,NextNode},{Medium}}] = ets:lookup(MapEts,MapEtsKey),
    io:format("Node: ~p NextNode: ~p~n",[Node,NextNode]),
    
    %case NextNode of
    %    NextNode when is_atom(NextNode) ->
    [{Node,{NodeLocation,_, _}}] = ets:lookup(NodesEts,Node),
    [{NextNode,{NextNodeLocation,_, _}}] = ets:lookup(NodesEts,NextNode),
    draw_route(DC, NodeLocation,NextNodeLocation, Medium),
    %    NextNode -> ok
    %end,
    switch_to_full_map(DC, ets:next(MapEts,MapEtsKey), MapEts, NodesEts).

%%%%
%%  Draws the map as seen from Node
%%%%
switch_to_node(_, _,'$end_of_table', _, _) -> ok;
switch_to_node(DC, Node,Node, NodesEts, {XNodeLocation, YNodeLocation}) ->

    io:format("switch_to_node 2 Node: ~p~n",[Node]),
    [{Node,{{X,Y}, _MediumMode,RoutingSet}}] = ets:lookup(NodesEts,Node),
    
    wxDC:drawCircle(DC, {X,Y}, 15),
    wxTextCtrl:setValue(XNodeLocation, integer_to_list(X)),
    wxTextCtrl:setValue(YNodeLocation, integer_to_list(Y)),
	wxDC:drawLabel(DC,atom_to_list(Node), {X-10,Y-10,X+50,Y+50}),


    draw_routs(DC, Node, {X,Y},NodesEts,RoutingSet),

    switch_to_node(DC, Node,ets:next(NodesEts,Node),NodesEts, {XNodeLocation, YNodeLocation});

switch_to_node(DC, Node,Key, NodesEts, {XNodeLocation, YNodeLocation}) ->
    io:format("switch_to_node3 Node: ~p Key ~p~n",[Node,Key]),

    [{Key,{{X,Y}, _MediumMode,_RoutingSet}}] = ets:lookup(NodesEts,Key),
    wxDC:drawCircle(DC, {X,Y}, 15),
    wxTextCtrl:setValue(XNodeLocation, integer_to_list(X)),
    wxTextCtrl:setValue(YNodeLocation, integer_to_list(Y)),
	wxDC:drawLabel(DC,atom_to_list(Key), {X-10,Y-10,X+50,Y+50}),
    switch_to_node(DC, Node,ets:next(NodesEts,Key),NodesEts, {XNodeLocation, YNodeLocation}).

%%%%
%%  Draws all routes the SelectedNode familiar
%%%%
draw_routs(_, _, _,_,[]) ->     io:format("draw_routs ends~n"),
ok;
draw_routs(DC, SelectedNode, Location, NodesEts,[{{destination, 0},_, _}|RoutingSet]) ->
	draw_routs(DC, SelectedNode, Location, NodesEts,RoutingSet);
draw_routs(DC, SelectedNode, Location, NodesEts,[{{destination, Node}, {next_address, Node}, {medium, Medium}}|RoutingSet]) ->

    AtomNode = list_to_atom("node_" ++ integer_to_list(Node)),
    %AtomNode = Node,
    %AtomNode = list_to_atom("node_" ++ integer_to_list(Node)),
    io:format("draw_routsB N: ~p~n~n",[AtomNode]),

    [{AtomNode,{NextLocation, _,_}}] = ets:lookup(NodesEts,AtomNode),

    draw_route(DC, Location,NextLocation, Medium),
    draw_routs(DC, SelectedNode, Location, NodesEts,RoutingSet);


draw_routs(DC, SelectedNode, Location, NodesEts,[{{destination, Node1}, {next_address, Node2}, {medium, Medium}}|RoutingSet]) ->
    
    %AtomNode1 = Node1,  % = list_to_atom("node_" ++ integer_to_list(Node1)),
    AtomNode1 = list_to_atom("node_" ++ integer_to_list(Node1)),
        io:format("draw_routsC N: ~p~n~n",[AtomNode1]),

    [{AtomNode1,{Location1, _,_}}] = ets:lookup(NodesEts,AtomNode1),
    AtomNode2 = list_to_atom("node_" ++ integer_to_list(Node2)),
    %AtomNode2 = Node2,% = list_to_atom("node_" ++ integer_to_list(Node2)),
    
    [{AtomNode2,{Location2,_,_}}] = ets:lookup(NodesEts,AtomNode2),

    draw_route(DC, Location,Location2, Medium),
    draw_route(DC, Location1,Location2, 0),

    draw_routs(DC, SelectedNode, Location, NodesEts,RoutingSet).


draw_circle(_,'$end_of_table', _)-> ok;
draw_circle(DC,Node, NodesEts)->
    [{Key,{{X,Y}, _MediumMode,_RoutingSet}}] = ets:lookup(NodesEts,Node),
    wxDC:drawCircle(DC, {X,Y}, 15),
    wxDC:drawLabel(DC,atom_to_list(Key), {X-10,Y-10,X+50,Y+50}),
    draw_circle(DC,ets:next(NodesEts,Node), NodesEts).

%%%%
%%  Draws a line (representing a communication line) from Location1 to Location2 according to Medium
%%%%
draw_route(DC, Location1,Location2, Medium) ->
    io:format("Medium: ~p~n",[Medium]),
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

configButtonUpdate(idle, [PlcOff,_,RfOff,_])->selectRadio(PlcOff,RfOff);
configButtonUpdate(rf_only, [PlcOff,_,_,RfOn])->selectRadio(PlcOff,RfOn);
configButtonUpdate(plc_only, [_,PlcOn,RfOff,_])->selectRadio(PlcOn,RfOff);
configButtonUpdate(dual, [_,PlcOn,_,RfOn])->selectRadio(PlcOn,RfOn).

selectRadio(Plc,Rf) -> wxRadioButton:setValue(Plc,true), wxRadioButton:setValue(Rf,true).

checkRadio(0,0) -> idle;
checkRadio(0,1) -> rf_only;
checkRadio(1,0) -> plc_only;
checkRadio(1,1) -> dual;
checkRadio(Plc,Rf) -> {wxRadioButton:getValue(Plc), wxRadioButton:getValue(Rf)}.
checkRadio([_,PLC,_,RF]) ->checkRadio(wxRadioButton:getValue(PLC), wxRadioButton:getValue(RF)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Create Form Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_radio_buttons(Panel) ->
    RadioButtonSizer =
        wxStaticBoxSizer:new(?wxVERTICAL, Panel,
            [{label, "wxRadioButton"}]),
    Buttons =
        [wxRadioButton:new(Panel, ?wxID_ANY, "PLC OFF",[{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "PLC ON", []),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF OFF",[{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF ON", [])],

    Fun =
        fun(Item) ->
            wxSizer:add(RadioButtonSizer, Item)
        end,
    wx:foreach(Fun, Buttons),
    {RadioButtonSizer,Buttons}.