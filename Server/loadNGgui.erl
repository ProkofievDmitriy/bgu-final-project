-module(loadNGgui).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(SERVER, ?MODULE).
-define(X_SIZE, 840).
-define(Y_SIZE, 840).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2,handle_cast/2, handle_call/3, handle_event/2, handle_sync_event/3]).

-record(state,
	{frame,panel, mapEts, nodesEts, canvas, log, nodeChoice, selectedNode = all, numberOfNodes, configButtons, locationInput}).
-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time, valid}).

%%%%%%%%%%%%
%%%     {{NodeNameAtom,NextNode},{Medium}}
%%%
%%%     {NodeNameAtom,{NodeNumber,Mode,RoutingSet}}
%%%%%%%%%%%%
start() ->
    io:format("start 1 ~n"),
    WxServer = wx:new(),
    io:format("Server: ~p~n",[WxServer]),
    wx_object:start_link({global, ?SERVER}, ?MODULE, WxServer, []).





init(WxServer) ->
    NodesEts = ets:new(nodesEts,[set,named_table]),
    MapEts = ets:new(mapEts,[set,named_table]),
	io:format("init 1 ~n"),

    Frame = wxFrame:new(WxServer, ?wxID_ANY, "LOADng", [{size,{?X_SIZE, ?Y_SIZE+40}}]),
    io:format("Frame: ~p~n",[Frame]),
    Panel = wxPanel:new(Frame),



    %% Setup sizers
    SuperSz = wxBoxSizer:new(?wxVERTICAL),
    TitleSz = wxBoxSizer:new(?wxVERTICAL),
    OuterSz  = wxBoxSizer:new(?wxHORIZONTAL),
    LogSz = wxBoxSizer:new(?wxVERTICAL),

    NodesSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Nodes:"}]),
    ManagementSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "Management:"}]),
    %NodesSz = wxBoxSizer:new(?wxVERTICAL),
    %ManagementSz = wxBoxSizer:new(?wxVERTICAL),

%% create widgets
    Title = wxStaticText:new(Panel, ?wxID_ANY,"LOADng Network Management Tool:",[{style, ?wxALIGN_CENTER}]),


    %%Setup Buttons:
    ButtonDeleteTable = wxButton:new(Panel, ?wxID_ANY, [{label,"Delete Routes Table"}]),
    ButtonSendMSG = wxButton:new(Panel, ?wxID_ANY, [{label,"Send message"}]),
    ButtonSendConfig = wxButton:new(Panel, ?wxID_ANY, [{label,"Send New Configurations"}]),
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
    wxListBox:insertItems(NodeChoice,["Show Full Map"],0),
    %wxChoice:setToolTip(Choice, "Node:"),


    %% Add to sizers

    %% Management:
    wxSizer:add(ManagementSz, NodeChoice),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, ButtonDeleteTable),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, ButtonSendMSG),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, RadioButtonSizer),
    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, ButtonSendConfig),
    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, XNodeLocation),
    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, YNodeLocation),
    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, UpdateLocation),


    %% Nodes:
    wxSizer:add(NodesSz, Canvas),


    %% LOGS:
    Log = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, ""}, {size,{?X_SIZE,100}},
                  {style, ?wxDEFAULT bor ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
    wxSizer:add(LogSz, Log, []),




    wxSizer:add(OuterSz, ManagementSz, []),
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

    {Frame,#state{frame=Frame,panel=Panel,mapEts = MapEts, nodesEts = NodesEts, log = Log,canvas = Canvas, nodeChoice = NodeChoice, numberOfNodes = 0, configButtons = ConfigButtons, locationInput =  {XNodeLocation, YNodeLocation}}}.








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
%% Handling events
%%
%% @end
%%--------------------------------------------------------------------
<<<<<<< Updated upstream
handle_event(#wx{event=#wxCommand{type = command_listbox_selected, cmdString="Show Full Map"}}, State = #state{log = Log, mapEts = MapEts, nodesEts = NodesEts, configButtons = ConfigButtons}) ->
    io:format("command_listbox_selected Full~n"),
    SelectedNode = all,
    DC = wxWindowDC:new(State#state.canvas),
    wxDC:clear(DC),

    switch_to_full_map(DC, ets:first(MapEts), MapEts, ets:first(NodesEts), NodesEts),

    configButtonUpdate(PLC,RF,ConfigButtons),
    wxWindowDC:destroy(DC),

  %  wxTextCtrl:appendText(Log, "Selected node ID: " ++ NodeName ++"\n"),
=======
handle_event(#wx{event=#wxCommand{type = command_listbox_selected, cmdString=Ex}}, State = #state{nodesEts = NodesEts, configButtons = ConfigButtons}) -> 
    io:format("command_listbox_selected ~p~n",[Ex]),
>>>>>>> Stashed changes

{noreply,State#state{selectedNode = SelectedNode}};



handle_event(#wx{event=#wxCommand{type = command_listbox_selected, cmdString=Ex}}, State = #state{log = Log, nodesEts = NodesEts, configButtons = ConfigButtons}) ->
    io:format("command_listbox_selected ~p~n",[Ex]),
    SelectedNode = list_to_atom(Ex),
    DC = wxWindowDC:new(State#state.canvas),
    wxDC:clear(DC),

    switch_to_node(DC, SelectedNode,ets:first(NodesEts), NodesEts, State#state.locationInput),
    [{SelectedNode,{_, _, {PLC,RF},_}}] = ets:lookup(NodesEts,SelectedNode),
    configButtonUpdate(PLC,RF,ConfigButtons),
    wxWindowDC:destroy(DC),

  %  wxTextCtrl:appendText(Log, "Selected node ID: " ++ NodeName ++"\n"),

{noreply,State#state{selectedNode = SelectedNode}};

handle_event(#wx{id=Id, event=#wxCommand{type=command_button_clicked}}, 
	     State = #state{selectedNode = SelectedNode, locationInput =  {XNodeLocation, YNodeLocation}}) ->
    io:format("Update location clicked Selected: SelectedNode: ~p~n", [SelectedNode]),
    
    [{NodeNameAtom,{NumberOfNodes, _, Mode,RoutingSet}}] = ets:lookup(State#state.nodesEts,SelectedNode),

    X = list_to_integer(wxTextCtrl:getLineText(XNodeLocation,0)),
    Y = list_to_integer(wxTextCtrl:getLineText(YNodeLocation,0)),


    io:format("Update location to ~p,~p SelectedNode: ~p~n", [X,Y, SelectedNode]),
    ets:insert(State#state.nodesEts,{NodeNameAtom,{NumberOfNodes, {X,Y}, Mode,RoutingSet}}),

{noreply,State};

handle_event(_, State) -> io:format("graphic handle_event nothing interesting~n"),
{noreply,State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------

handle_cast({node_is_up,{NodeNameAtom, Data}}, State = #state{log = Log, nodeChoice = NodeChoice, numberOfNodes = NumberOfNodes}) ->
	NodeName = atom_to_list(NodeNameAtom),
	RoutingSet = proplists:get_value(routing_set, Data),
	MediumMode = proplists:get_value(medium_mode, Data),

	% Update radio buttons status: 
	Mode = case MediumMode of
		plc_only -> {1,0};
		rf_only -> {0,1};
		dual -> {1,1};
	 	idle -> {0,0} end,
	Res = ets:lookup(State#state.nodesEts,NodeNameAtom),
	case Res of
		[] ->
			io:format("loadNGgui.erl: node_is_up: ID=~p Res: ~p~n",[NodeName,Res]),
		    %wxTextCtrl:appendText(Log, "New node is up ID: " ++ NodeName ++"\n"),
		    wxListBox:insertItems(NodeChoice,[NodeName],0),
		    ets:insert(State#state.nodesEts,{NodeNameAtom,{NumberOfNodes+1, {50,50}, Mode,RoutingSet}}),
		    {noreply, State#state{numberOfNodes = NumberOfNodes + 1 }};
		[{NodeNameAtom,{NodeNumber, Location, _,_}}] ->
			io:format("loadNGgui.erl: node updated: ID=~p Res:~p~n",[NodeNameAtom,Res]),
			%wxTextCtrl:appendText(Log, "Node ID: " ++ NodeName ++" updated\n"),
			ets:insert(State#state.nodesEts,{NodeNameAtom,{NodeNumber, Location, Mode,RoutingSet}}),
			{noreply, State};
		A ->
			io:format ("RES ~p~n",[A]),
			{noreply, State}

		end;

%handle_cast({update_state,{NodeName,{{PLC,RF},RoutingSet}}}, State = #state{log = Log}) ->
%    io:format("loadNGgui.erl: node updated: ID=~p~n",[NodeName]),
%    wxTextCtrl:appendText(Log, "Node ID: " ++ NodeName ++" updated\n"),
%    [{NodeName,{NodeNumber,_,_}}] = ets:lookup(State#state.nodesEts,NodeName),
%    ets:insert(State#state.nodesEts,{NodeName,{NodeNumber,{PLC,RF},RoutingSet}}),
%    {noreply, State};

handle_cast({printNodes}, State) ->
    io:format("loadNGgui.erl: print nodesEts:~n"),
    printNodesEts(ets:first(State#state.nodesEts), State#state.nodesEts),
    {noreply, State};

handle_cast(_A, State) ->
	io:format("loadNGgui.erl: handle_info cast: MSG=~p~n",[_A]),
	{noreply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%%--------------------------------------------------------------------
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
update_map(_, _, [])-> ok;
update_map(Ets, Node, [#routing_set_entry{dest_addr = NextNode, next_addr = NextNode, medium = Medium}|NodeRoutingMap])->
    ets:insert(Ets,{{Node,NextNode},{Medium}}),
    update_map(Ets, Node, NodeRoutingMap).
%{node_10,{0,{1,1},[]}}



<<<<<<< Updated upstream
switch_to_node(_, _,'$end_of_table', _, _) -> ok;
switch_to_node(DC, Node,Node, NodesEts, {XNodeLocation, YNodeLocation}) ->
    [{Node,{NodeNumber, {X,Y}, {PLC,RF},RoutingSet}}] = ets:lookup(NodesEts,Node),
    wxDC:drawCircle(DC, {X,Y}, 15),
    wxTextCtrl:setValue(XNodeLocation, integer_to_list(X)),
    wxTextCtrl:setValue(YNodeLocation, integer_to_list(Y)),
=======
switch_to_node(_, _,'$end_of_table', _) -> ok;
switch_to_node(DC, Node,Node, NodesEts) ->
    wxDC:drawCircle(DC, {300, 200}, 15),
    [{Node,{_NodeNumber, {_PLC,_RF},RoutingSet}}] = ets:lookup(NodesEts,Node),
    draw_routs(DC, Node, NodesEts,RoutingSet),
    switch_to_node(DC, Node,ets:next(NodesEts,Node),NodesEts);
>>>>>>> Stashed changes

	wxDC:drawLabel(DC,atom_to_list(Node), {X-10,Y-10,X+50,Y+50}),


    draw_routs(DC, Node, {X,Y},NodesEts,RoutingSet),
    switch_to_node(DC, Node,ets:next(NodesEts,Node),NodesEts, {XNodeLocation, YNodeLocation});

switch_to_node(DC, Node,Key, NodesEts, {XNodeLocation, YNodeLocation}) ->
    [{Key,{NodeNumber, {X,Y}, {_PLC,_RF},_RoutingSet}}] = ets:lookup(NodesEts,Key),

    wxDC:drawCircle(DC, {X,Y}, 15),
    wxTextCtrl:setValue(XNodeLocation, integer_to_list(X)),
    wxTextCtrl:setValue(YNodeLocation, integer_to_list(Y)),
	wxDC:drawLabel(DC,atom_to_list(Key), {X-10,Y-10,X+50,Y+50}),


    switch_to_node(DC, Node,ets:next(NodesEts,Key),NodesEts, {XNodeLocation, YNodeLocation}).


draw_routs(_, _, _,_,[]) -> ok;
draw_routs(DC, SelectedNode, Location, NodesEts,[#routing_set_entry{dest_addr = Node, next_addr = Node, medium = Medium}|RoutingSet]) ->
    io:format("draw_routs N: ~p~n~n",[Node]),
    AtomNode = list_to_atom("node_" ++ integer_to_list(Node)),
    [{AtomNode,{NodeNumber, NextLocation, _,_}}] = ets:lookup(NodesEts,AtomNode),

    case Medium of
        2 -> %PLC
            Colour = ?wxBLUE;
        1 -> %RF
            Colour = ?wxRED;
        _ ->
            Colour = ?wxBLACK
    end,

    Pen = wxPen:new({200,200,0,255}),
    wxPen:setColour(Pen, Colour),
    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
    wxDC:setPen(DC, Pen),

    wxDC:drawLine(DC, Location, NextLocation),
    draw_routs(DC, SelectedNode, Location, NodesEts,RoutingSet);

draw_routs(DC, SelectedNode, Location, NodesEts,[#routing_set_entry{dest_addr = Node1, next_addr = Node2, medium = Medium}|RoutingSet]) ->
    io:format("draw_routs N: ~p~n~n",[Node1]),
    AtomNode1 = list_to_atom("node_" ++ integer_to_list(Node1)),
    [{AtomNode1,{NodeNumber, Location1, _,_}}] = ets:lookup(NodesEts,AtomNode1),
    AtomNode2 = list_to_atom("node_" ++ integer_to_list(Node2)),
    [{AtomNode2,{NodeNumber2, Location2,_,_}}] = ets:lookup(NodesEts,AtomNode2),



    Pen = wxPen:new({200,200,0,255}),
    wxPen:setColour(Pen, ?wxBLACK),
    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
    wxDC:setPen(DC, Pen),
   

    wxDC:drawLine(DC, Location1, Location2),

    draw_routs(DC, SelectedNode, Location, NodesEts,RoutingSet).

configButtonUpdate(0,0,[PlcOff,_,RfOff,_])->selectRadio(PlcOff,RfOff);
configButtonUpdate(0,1,[PlcOff,_,_,RfOn])->selectRadio(PlcOff,RfOn);
configButtonUpdate(1,0,[_,PlcOn,RfOff,_])->selectRadio(PlcOn,RfOff);
configButtonUpdate(1,1,[_,PlcOn,_,RfOn])->selectRadio(PlcOn,RfOn).

selectRadio(Plc,Rf) -> wxRadioButton:setValue(Plc,true), wxRadioButton:setValue(Rf,true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Create Form Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_radio_buttons(Panel) ->
    RadioButtonSizer =
        wxStaticBoxSizer:new(?wxVERTICAL, Panel,
            [{label, "wxRadioButton"}]),
    Buttons =
        [wxRadioButton:new(Panel, ?wxID_ANY, "PLC OFF",
            [{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "PLC ON", []),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF OFF",
            [{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF ON", [])],

    Fun =
        fun(Item) ->
            wxSizer:add(RadioButtonSizer, Item)
        end,
    wx:foreach(Fun, Buttons),

    {RadioButtonSizer,Buttons}.





