-module(loadNGgui).
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(SERVER, ?MODULE).
-define(X_SIZE, 840).
-define(Y_SIZE, 840).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2,handle_cast/2, handle_call/3, handle_event/2]).

-record(state, 
	{frame,panel,nodesEts, log}).
	 
start() ->
    io:format("start 1 ~n"),
    WxServer = wx:new(),
    io:format("Server: ~p~n",[WxServer]),
    wx_object:start_link({global, ?SERVER}, ?MODULE, WxServer, []).





init(WxServer) ->
    E = ets:new(tab,[set,named_table, public]),

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
    %Win = wxWindow:new(Panel, ?wxID_ANY, [{size, {600,400}}]),
    Win = wxPanel:new(Panel, [{size, {600,400}}]    ),

    %% Radio Buttons:
    RadioButtonSizer = create_radio_buttons(Panel),

    %% Add to sizers

    %% Management:
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, ButtonDeleteTable),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, ButtonSendMSG),
    wxSizer:addSpacer(ManagementSz, 20),
    wxSizer:add(ManagementSz, RadioButtonSizer),
    wxSizer:addSpacer(ManagementSz, 10),
    wxSizer:add(ManagementSz, ButtonSendConfig),



    %% Nodes:
    wxSizer:addSpacer(NodesSz, 100),
    wxSizer:add(NodesSz, Win),
    wxSizer:addSpacer(NodesSz, 100),


    %% LOGS:
    Log = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, ""}, {size,{?X_SIZE,50}},
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

    {Frame,#state{frame=Frame,panel=Panel,nodesEts = E, log = Log}}. 








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
handle_event(_, State) -> io:format("graphic handle_event nothing interesting~n"),
{noreply,State}.	 
	
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% 
%% @end
%%--------------------------------------------------------------------

handle_cast({node_is_up,{Id,RF,PLC}}, State = #state{log = Log}) -> 
    wxTextCtrl:appendText(Log, "New node is up: ID~n"),
    io:format("loadNGgui.erl: node_is_up: ID=~p RF:~p PLC: ~p~n",[Id,RF,PLC]),
    ets:insert(State#state.nodesEts,{Id,{{RF,PLC},{5,5}}}),
    {noreply, State};

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
  io:format("terminat, reason=~p ",[_Reason]),
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
create_radio_buttons(Panel) ->
    RadioButtonSizer = 
        wxStaticBoxSizer:new(?wxVERTICAL, Panel,
            [{label, "wxRadioButton"}]),
    Buttons =
        [wxRadioButton:new(Panel, ?wxID_ANY, "RF OFF",
            [{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "RF ON", []),
        wxRadioButton:new(Panel, ?wxID_ANY, "PLC OFF",
            [{style, ?wxRB_GROUP}]),
        wxRadioButton:new(Panel, ?wxID_ANY, "PLC ON", [])],

    Fun =
        fun(Item) ->
            wxSizer:add(RadioButtonSizer, Item)
        end,
    wx:foreach(Fun, Buttons),

    RadioButtonSizer.
