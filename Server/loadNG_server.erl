%%%-------------------------------------------------------------------
%%% @author Deddy Zagury
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%%   
%%% @end
%%% Created : 15. Jan 2017 1:40 AM
%%%-------------------------------------------------------------------
-module(loadNG_server).
-author("Deddy Zagury").

%%% -behaviour(gen_server).
-behaviour(wx_object).
-include_lib("wx/include/wx.hrl").



-define(X_SIZE, 600).
-define(Y_SIZE, 600).



%% API
-export([start/0]).

%% Callbacks
-export([init/1, terminate/2, code_change/3,
	  handle_sync_event/3, handle_event/2,handle_cast/2, handle_info/2, 
	 handle_call/3]).

-define(SERVER, ?MODULE).
-define(wxGC, wxGraphicsContext).	 


-record(loadNG_state,{frame,panel}).  %% Listeners or callbacks


start() ->
    wx_object:start({global, ?SERVER},?MODULE, [], []).



init(_)->
	WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "loadNG management", [{size,{?X_SIZE, ?Y_SIZE+40}}]),
    Panel  = wxPanel:new(Frame),
	wxFrame:show(Frame), 
   	Pen = wxPen:new(),
	{Frame,#loadNG_state{frame=Frame,panel=Panel}}. 








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
handle_cast(_A, State) -> 
	%io:format("spidErlang_serverT.erl: handle_info cast: MSG=~p~n",[_A]),	
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
terminate(_Reason, #loadNG_state{frame=Frame}) ->
  io:format("terminat, reason=~p ~n",[_Reason]),
  wxWindow:destroy(Frame),
    ok.

code_change(_, _, State) -> 
	io:format("code_change ~n"),
	{noreply, State}.