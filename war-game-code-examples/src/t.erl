%% @author dmitriy
%% @doc @todo Add description to t.


-module(t).


-include_lib("wx/include/wx.hrl"). 
-include_lib("wx/include/gl.hrl"). 

-include("../include/params.hrl").

-record(state, 
	{
	  parent,
	  config,
	  canvas,
	  current_selected_team,
	  current_selected_unit,
	  gl_context,
	  timer,
	  time,
	  red_team_stat,
	  blue_team_stat
	}).

-record(img, {
       width,       % the height of this image, in pixels.
       height,      % the width of this image, in pixels.
       drawWidth,   % the UV map width position for when this is draw and mapped to a quad
       drawHeight,  % the UV map height position for when this is mapped to a quad
       glTextureID % the internal OpenGL texture number
}).

%% wx_object callbacks
-export([init/1, terminate/2,  code_change/3,
	 handle_info/2, handle_call/3, handle_cast/2, handle_event/2, handle_sync_event/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0 , stop/0]).

stop()->
	wx_object:stop(?MODULE).

start() ->
   	WX		= wx:new(),
    wx_object:start_link(?MODULE, WX, []).

%% ====================================================================
%% wx_object callback functions
%% ====================================================================
init(WX) ->
    wx:batch(fun() -> init_frame(WX, ?WINDOW_WIDTH, ?WINDOW_HEIGHT, "War Game Simulator") end).

init_frame(WX, Width, Height, Title) ->
    process_flag(trap_exit, true),
	Frame = wxFrame:new( WX, 1, Title, [] ),
	Panel = wxPanel:new(Frame, []),
	
    Size = {Width, Height},
    % create GL Canvas
 	CanvasSize = {?CANVAS_WIDTH, ?CANVAS_HEIGHT},
    GLAttrs = [
			  	 {size, CanvasSize},
            	 {style, ?wxFULL_REPAINT_ON_RESIZE},
			 	 {attribList, [?WX_GL_RGBA,
			      ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_MIN_RED,8,
			      ?WX_GL_MIN_GREEN,8,
			      ?WX_GL_MIN_BLUE,8,
			      ?WX_GL_DEPTH_SIZE,24,0]}
    ],
    GL = wxGLCanvas:new(Panel, GLAttrs),
	wxGLCanvas:connect(GL, paint, [callback]),    
 	wxGLCanvas:connect(GL, size),
	wxPanel:connect(GL, left_down),
	
	Cursor = wxCursor:new(?wxCURSOR_CROSS),
    wxWindow:setCursor(GL,  Cursor),

	ButtSz = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Controls"}]),
    Labels = [{?B6_LABEL,?B6_ID}, {?B7_LABEL,?B7_ID}],
    Buttons = [wxButton:new(Panel, Id, [{label, L}])|| {L,Id} <- Labels],
    %% Add to sizers
    [wxSizer:add(ButtSz, Button) || Button <- Buttons],
    wxPanel:connect(Panel, command_button_clicked),

    Units = [?B1_LABEL, ?B2_LABEL, ?B3_LABEL, ?B4_LABEL, ?B5_LABEL],
    RadioBoxUnits = wxRadioBox:new(Panel, 1, "Choose Unit to Spawn",
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      Units,
			      [{majorDim, 3},
			       {style, ?wxHORIZONTAL}]),
    Teams = [?BLUE_TEAM_LABEL, ?RED_TEAM_LABEL],
    RadioBoxTeams = wxRadioBox:new(Panel, 1, "Choose Team",
			      ?wxDefaultPosition,
			      ?wxDefaultSize,
			      Teams,
			      [{majorDim, 3},
			       {style, ?wxHORIZONTAL}]),
	
	
    % sizing
    wxWindow:setSize(GL, CanvasSize),

    MainSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:setMinSize(MainSz, Size),	
	wxSizer:fit(MainSz, Frame),
	
	CanvasSz = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label , "Main Window"}]),
	wxSizer:add(CanvasSz, GL, [{flag, ?wxALL},{proportion, 1}]),	
	
	StatisticsSz 	= wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "Statistics"}]),
	BlueTeamTxtCtrl = wxTextCtrl:new(Panel, ?BLUE_TEAM_STAT_ID,[{value,"This is a\nmultiline\nBlueTeamTxtCtrl"},{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
	RedTeamTxtCtrl  = wxTextCtrl:new(Panel, ?RED_TEAM_STAT_ID, [{value,"This is a\nmultiline\nRedTeamTxtCtrl"}, {style, ?wxDEFAULT bor ?wxTE_MULTILINE}]), 	
	wxTextCtrl:setEditable(RedTeamTxtCtrl,false),
 	wxTextCtrl:setEditable(BlueTeamTxtCtrl,false),
	
	wxSizer:add(StatisticsSz, BlueTeamTxtCtrl, [{border, 0},{flag, ?wxEXPAND},{proportion, 1}]),
	wxSizer:add(StatisticsSz, RedTeamTxtCtrl, [{border, 0},{flag, ?wxEXPAND},{proportion, 1}]),
	wxSizer:add(StatisticsSz, ButtSz, [{border, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(StatisticsSz, RadioBoxUnits,[{border, 0}, {flag, ?wxEXPAND}]),   
    wxSizer:add(StatisticsSz, RadioBoxTeams,[{border, 0}, {flag, ?wxEXPAND}]),   
	
	wxSizer:add(MainSz, StatisticsSz, [{border, 0}, {flag, ?wxEXPAND}]),
    wxSizer:add(MainSz, CanvasSz, [{border, 0}, {flag, ?wxEXPAND}]),

    
	wxPanel:setSizer(Panel, MainSz),
    wxSizer:layout(MainSz),	
    
    % disables frame resize
 %    set_comp_size( Frame, wxWindow:getSize(Frame)),

    % final display code
    wxWindow:show(Frame),
    wxWindow:setFocus(Frame),
    wxGLCanvas:setCurrent(GL),
    init_gl(GL),

    
    wxWindow:raise(Frame),
    
    % return
    Timer = timer:send_interval(?TIMER_INTERVAL, self(), update), % ~50 fps
    {GL, #state{parent = Frame, 
				   	config = WX, 
					canvas = GL, 
				 	gl_context = init_gl_context(GL),
		  		    timer = Timer,
					current_selected_unit = RadioBoxUnits,
					current_selected_team = RadioBoxTeams,
			   		red_team_stat = RedTeamTxtCtrl, 
					blue_team_stat = BlueTeamTxtCtrl
			   }}.


handle_sync_event(_PaintEvent, _, #state{canvas=Canvas}) ->
    wxGLCanvas:setCurrent(Canvas),
    DC = wxPaintDC:new(Canvas),
    wxPaintDC:destroy(DC),
    ok.



% Buttons event handling
handle_event(#wx{id = ID, event = #wxCommand{type = command_button_clicked}} , State = #state{}) ->
%% 	?DEBUG("Event command_button_clicked ID=~p~n",[ID]),
	case ID of 
		?B7_ID ->  %Start Game pressed
			?DATA_SERVER:game_start();
		?B6_ID -> % Kill aLL pressed
			?DATA_SERVER:kill_all()
		  end,
    {noreply, State};

% Mouse event - left button pressed handling
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State = #state{}) ->
	% Read Radio buttons configuration to determine which unit to which team shold e added
	Unit = wxRadioBox:getString(State#state.current_selected_unit,wxRadioBox:getSelection(State#state.current_selected_unit)),
	Team = wxRadioBox:getSelection(State#state.current_selected_team) + 1,
	
%% 	?DEBUG("Event left_down : X=~p, Y=~p, CurrentUnit=~p, CurrentTeam=~p~n",[X,Y,Unit,Team]),
	case Unit of 	
		?B1_LABEL -> 
			?DATA_SERVER:spawn_new_unit([tank, Team, [X,Y]], 1);
		?B2_LABEL ->
			?DATA_SERVER:spawn_new_unit([soldier, Team, [X,Y]], 1);
		?B3_LABEL -> 
			?DATA_SERVER:spawn_new_unit([mines, 0, [X,Y]], 1);
		?B4_LABEL ->
			?DATA_SERVER:spawn_new_unit([resource, 0, [X,Y]], 1);
		?B5_LABEL ->
			?DATA_SERVER:spawn_new_unit([harvester, Team, [X,Y]], 1)
		  end,
    {noreply, State};

% Sample ov event handling
handle_event(Ev = #wx{}, State = #state{}) ->
     ?DEBUG("Got Event ~p\n", [Ev]),
    {noreply, State}.
	
%% Callbacks handled as normal gen_server callbacks
handle_info(update, State=#state{canvas = Canvas, gl_context=GL}) ->
   	wx:batch(fun() ->  
		draw_sim_time(State, Canvas , GL), % Draw imulation time
		update_teams_status(State), % Update teams status 
		redraw(GL), % redraw full frame
		wxGLCanvas:swapBuffers(Canvas), 
		gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT)
			 end),
    {noreply, State};

% Sample Info event handling
handle_info(Msg, State) ->
    ?DEBUG("Got Info ~p\n", [Msg]),
    {noreply, State}.

%Proper shutdown handling
handle_call(shutdown, _From, State=#state{parent=Panel}) ->
    catch wxGLCanvas:destroy(State#state.canvas),
    timer:cancel(State#state.timer),
    wxPanel:destroy(Panel),
    {stop, normal, ok, State};	

%Sample call handling
handle_call(Msg, _From, State) ->
    ?DEBUG("Got Call ~p\n", [Msg]),
    {reply,{error, nyi}, State}.
%Sample cast call
handle_cast(Msg, State) ->
    ?DEBUG("Got cast ~p~n",[Msg]),
    {noreply,State}.

code_change(_, _, State) ->
    {stop, ignore, State}.

terminate(_Reason, _) ->
    ok.

%%%% BUild Frame Functions
draw_background([], []) -> ok; 
draw_background(Animations, Locations) -> 
	[H|T] = Animations,
	Loc1 = lists:nth(1,Locations),
	Loc2 = lists:nth(2,Locations),
	Loc3 = lists:nth(3,Locations),
	draw_image_rotated(H , Loc1, 0),
	draw_image_rotated(H , Loc2, 120),
	draw_image_rotated(H , Loc3, 240),
	draw_background(T, Locations -- [Loc1,Loc2,Loc3]), 	
	ok.
	


draw_sim_time(_State, _Canvas , GL)->
	StartTime = GL#gl_context.startTime, %get start time from state of server
	GameTime = erlang:system_time(seconds) - StartTime,
	destroy_image(GL#gl_context.clockImg), 
	_ = GL#gl_context{clockImg=load_texture_by_string("Simulation Time : " ++ [?PAD(GameTime/60), $:, ?PAD(GameTime rem 60)])},
	draw_image(GL#gl_context.clockImg , [?WORLD_WIDTH/2, 10]).


update_teams_status(State)-> 
	RedStat = State#state.red_team_stat,
	BlueStat = State#state.blue_team_stat,
	
	[{{?BLUE_TEAM, ?GRAPHICS_SERVER} ,BlueRecord}|[]] = ets:lookup(?LOCAL_STORAGE, {?BLUE_TEAM, ?GRAPHICS_SERVER} ),
	[{{?RED_TEAM, ?GRAPHICS_SERVER}, RedRecord}|[]] = ets:lookup(?LOCAL_STORAGE, {?RED_TEAM, ?GRAPHICS_SERVER} ),
	
	wxTextCtrl:clear(RedStat),
	wxTextCtrl:clear(BlueStat),
	wxTextCtrl:changeValue(RedStat, ?STAT_FORMAT(RedRecord)),
	wxTextCtrl:changeValue(BlueStat, ?STAT_FORMAT(BlueRecord)).


redraw(#gl_context{animations=Animations})->
	draw_background(Animations#animations.backgrounds, ?BACKGROUD_LOCATIONS), % first of all draw backgrounds
	TableList = ets:tab2list(?DBNAME), % convert table to list
	lists:foreach( fun({Key, Value=#params{}}) -> %draw firs layer 
			case Value#params.type of
				  colis ->
					  draw_colis(Key, Value, Animations);
				  soldier_dead ->
					  draw_soldier_dead(Key, Value, Animations);
				  collected ->
					  draw_resource_collected(Key, Value, Animations);
				  footprints ->
					  draw_footprints(Key, Value, Animations);
				  resource ->
					  draw_resource(Key, Value, Animations);
				  mines ->
					  draw_mine(Key, Value, Animations);
				_-> ok
			  end
  end, TableList),
	lists:foreach( fun({Key, Value=#params{}}) ->  % draw second layer 
			case Value#params.type of
				  tank ->
					  draw_tank(Key, Value, Animations);
				  explosion ->
					  draw_explosion(Key, Value, Animations);
				  soldier ->
					  draw_soldier(Key, Value, Animations);
				  harvester ->
					  draw_harvester(Key, Value, Animations);
				  base ->
					  draw_base(Key, Value, Animations);
				  soldier_flash ->
					  draw_soldier_flash(Key, Value, Animations);
				  tank_flash ->
					  draw_tank_flash(Key, Value, Animations);				
				  victory ->
					  draw_static_img(Key, Value, Animations#animations.victory);				
				  defeat ->
					  draw_static_img(Key, Value, Animations#animations.defeat);				
				_-> ok
			  end
  end, TableList).

draw_static_img(_Key, Params, Animation)-> 
	draw_image(lists:nth(Params#params.img, Animation), Params#params.location).

draw_base(Key, Params, Animations)->
	case Params#params.team of
		?BLUE_TEAM -> Animation = Animations#animations.blue_base;
		?RED_TEAM -> Animation = Animations#animations.red_base
	end,	
	
	case Params#params.state of
		?STATE_DYING ->
			  [X,Y] = Params#params.location,
			  ?DATA_SERVER:insert_debug_unit( { {{X,Y},g}, create_debug_params(?STATE_DYING, explosion, 0, [X,Y], 0, 0, [] ) } ),
			  ?DATA_SERVER:insert_debug_unit( { {{X+10,Y+10},g}, create_debug_params(?STATE_DYING, explosion, 0, [X+10,Y+10], 0, 0, [] ) } ),
			  ?DATA_SERVER:insert_debug_unit( { {{X-10,Y+10},g}, create_debug_params(?STATE_DYING, explosion, 0, [X-10,Y+10], 0, 0, [] ) } ),
			  ?DATA_SERVER:insert_debug_unit( { {{X-10,Y-10},g}, create_debug_params(?STATE_DYING, explosion, 0, [X-10,Y-10], 0, 0, [] ) } ),
			  ?DATA_SERVER:insert_debug_unit( { {{X+10,Y-10},g}, create_debug_params(?STATE_DYING, explosion, 0, [X+10,Y-10], 0, 0, [] ) } ),
			  case Params#params.team of
				?BLUE_TEAM -> VictoryLocation = [?WORLD_WIDTH_MIDDLE + ?WORLD_WIDTH_MIDDLE/2, ?WORLD_HEIGHT_MIDDLE + ?WORLD_HEIGHT_MIDDLE/2],
			  				  Defeatlocation = [?WORLD_WIDTH_MIDDLE/2, ?WORLD_HEIGHT_MIDDLE/2];
				?RED_TEAM -> Defeatlocation = [?WORLD_WIDTH_MIDDLE + ?WORLD_WIDTH_MIDDLE/2, ?WORLD_HEIGHT_MIDDLE + ?WORLD_HEIGHT_MIDDLE/2],
			  				 VictoryLocation = [?WORLD_WIDTH_MIDDLE/2, ?WORLD_HEIGHT_MIDDLE/2]
			end,	  
			  ?DATA_SERVER:insert_debug_unit( { {{X+100,Y-100},g}, create_debug_params(?STATE_DYING, victory, 0, VictoryLocation, 0, 0, [] ) } ),
			  ?DATA_SERVER:insert_debug_unit( { {{X+1050,Y-1050},g}, create_debug_params(?STATE_DYING, defeat, 0, Defeatlocation, 0, 0, [] ) } ),
			ets:delete_object(?DBNAME, {Key, Params});
		_ -> 
			draw_image(lists:nth(Params#params.img, Animation), Params#params.location),
			ets:insert(?DBNAME, {Key, update_params(Params, Animation) })
end.

draw_mine(Key, Params, Animations)->
	case Params#params.state of
		?STATE_DYING ->
			[X,Y] = Params#params.location,
		 	 ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, explosion, 0, [X,Y], 0, 0, [] ) } ),
			ets:delete_object(?DBNAME, {Key, Params});
		?STATE_FIND_ENEMIES -> 
			draw_image(lists:nth(Params#params.img, Animations#animations.mines), Params#params.location),
			ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.mines) })
end.

draw_resource(Key, Params, Animations)->
	#resource_p{type=Type} = Params#params.others, %% change to resource_p
	case Type of
		gold -> Animation = Animations#animations.gold_coin;
		iron -> Animation = Animations#animations.silver_coin
	end,
	
	case Params#params.state of
		?STATE_DYING ->
			[X,Y] = Params#params.location,
		 	 ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, collected, 0, [X,Y], 0, 0, [] ) } ),
			ets:delete_object(?DBNAME, {Key, Params});
		_ -> 
			draw_image(lists:nth(Params#params.img, Animation), Params#params.location),
			ets:insert(?DBNAME, {Key, update_params(Params, Animation) })
end.

draw_resource_collected(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.collect_resource), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.collect_resource) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.collect_resource) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

draw_explosion(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.explosion), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.explosion) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.explosion) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

draw_soldier_dead(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.soldier_dead), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.soldier_dead) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.soldier_dead) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

draw_colis(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.tank_colis), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.tank_colis) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.tank_colis) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.


draw_tank_flash(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.tank_fire), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.tank_fire) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.tank_fire) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

draw_tank(Key, Params=#params{}, Animations) ->
	case Params#params.team of
		?BLUE_TEAM -> Animation = Animations#animations.blue_tank;
		?RED_TEAM -> Animation = Animations#animations.red_tank
	end,
	
	case Params#params.state of
		?STATE_DYING ->
			[X,Y] = Params#params.location,
		 	 ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, explosion, 0, [X,Y], 0, 0, [] ) } ),
			
			[{{_Team , ?GRAPHICS_SERVER} ,TeamStat}] = ets:lookup(?LOCAL_STORAGE, {Params#params.team , ?GRAPHICS_SERVER}),
			Killed = TeamStat#team_stat.units_killed,
			ets:insert(?LOCAL_STORAGE, {{Params#params.team , ?GRAPHICS_SERVER}, TeamStat#team_stat{units_killed=Killed+1}}),
			
			 ets:delete_object(?DBNAME, {Key, Params});
		?STATE_SEEK ->
			  [X,Y] = Params#params.location,
			  ColisLocation = [X-math:cos(to_radians(Params#params.direction))*20,Y-math:sin(to_radians(Params#params.direction))*15],
			  ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, colis, 0, ColisLocation, Params#params.direction, 0, [] ) } ),
			  
			  draw_image_rotated(lists:nth(Params#params.img, Animation), Params#params.location, Params#params.direction),
			  ets:insert(?DBNAME, {Key, update_params(Params, Animation) });
		?STATE_ATTACK ->
			  [X,Y] = Params#params.location,
			  ShootEffectLocation = [X+math:cos(to_radians(Params#params.direction))*30,Y+math:sin(to_radians(Params#params.direction))*25],
			  draw_image_rotated(lists:nth(Params#params.img, Animation), Params#params.location, Params#params.direction),
			  case ets:lookup(?DBNAME, {{X,Y}, g}) of
				  [] ->
			  		?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, tank_flash, 0, ShootEffectLocation, Params#params.direction, 0, [] ) } );
				  _else -> ok
			  end,
			  ets:insert(?DBNAME, {Key, update_params(Params, Animation) })
	end.


draw_harvester(Key, Params=#params{}, Animations) ->
	case Params#params.team of
		?BLUE_TEAM -> Animation = Animations#animations.blue_harvester;
		?RED_TEAM -> Animation = Animations#animations.red_harvester
	end,
	
	case Params#params.state of
		?STATE_DYING ->
			[X,Y] = Params#params.location,
		 	 ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, explosion, 0, [X,Y], 0, 0, [] ) } ),
			[{{_Team , ?GRAPHICS_SERVER} ,TeamStat}] = ets:lookup(?LOCAL_STORAGE, {Params#params.team , ?GRAPHICS_SERVER}),
			Killed = TeamStat#team_stat.units_killed,
			ets:insert(?LOCAL_STORAGE, {{Params#params.team , ?GRAPHICS_SERVER}, TeamStat#team_stat{units_killed=Killed+1}}),
			ets:delete_object(?DBNAME, {Key, Params});
		_ ->
					  [X,Y] = Params#params.location,
					  ColisLocation = [X-math:cos(to_radians(Params#params.direction))*20,Y-math:sin(to_radians(Params#params.direction))*15],
					  ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, colis, 0, ColisLocation, Params#params.direction, 0, [] ) } ),
					  
					  draw_image_rotated(lists:nth(Params#params.img, Animation), Params#params.location, Params#params.direction),
					 ets:insert(?DBNAME, {Key, update_params(Params, Animation) })
	end.

draw_footprints(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.footprints), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.footprints) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.footprints) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

draw_soldier_flash(Key, Params=#params{}, Animations)->
		case Params#params.state of
		?STATE_DYING ->
			  draw_image_rotated(lists:nth(Params#params.img, Animations#animations.soldier_fire), Params#params.location, Params#params.direction),
			  if Params#params.img >= length(Animations#animations.soldier_fire) -> 
			  		  ets:insert(?DBNAME, {Key, Params#params{state=?DEAD}});
				  true -> 
					  ets:insert(?DBNAME, {Key, update_params(Params, Animations#animations.soldier_fire) })
			  end;
		?DEAD -> 
			ets:delete_object(?DBNAME, {Key, Params})
		end.

%Draw soldier by team and state
draw_soldier(Key, Params=#params{}, Animations) ->
	case Params#params.team of % choose animation by team
		?BLUE_TEAM -> Animation = Animations#animations.blue_soldier;
		?RED_TEAM -> Animation = Animations#animations.red_soldier
	end,
	
	case Params#params.state of % choose state of animation to draw
		?STATE_DYING ->
			[X,Y] = Params#params.location,
		 	?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, soldier_dead, 0, [X,Y], 0, 0, [] ) } ),
			[{{_Team , ?GRAPHICS_SERVER} ,TeamStat}] = ets:lookup(?LOCAL_STORAGE, {Params#params.team , ?GRAPHICS_SERVER}),
			Killed = TeamStat#team_stat.units_killed,
			ets:insert(?LOCAL_STORAGE, {{Params#params.team , ?GRAPHICS_SERVER}, TeamStat#team_stat{units_killed=Killed+1}}),
			ets:delete_object(?DBNAME, {Key, Params});
		?STATE_SEEK ->
					  [X,Y] = Params#params.location,
					  FootprintLocation = [X-math:cos(to_radians(Params#params.direction))*15,Y-math:sin(to_radians(Params#params.direction))*10],
					  draw_image_rotated(lists:nth(Params#params.img, Animation), Params#params.location, Params#params.direction),
					  ?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, footprints, 0, FootprintLocation, Params#params.direction, 0, [] ) } ),
					  ets:insert(?DBNAME, {Key, update_params(Params, Animation) });
		?STATE_ATTACK ->
					  [X,Y] = Params#params.location,
					  ShootEffectLocation = [X+math:cos(to_radians(Params#params.direction))*20,Y+math:sin(to_radians(Params#params.direction))*10],
					  draw_image_rotated(lists:nth(Params#params.img, Animation), Params#params.location, Params#params.direction),
			  		  case ets:lookup(?DBNAME, {{X,Y}, g}) of
						  []->
					  		?DATA_SERVER:insert_debug_unit( { {{X,Y}, g}, create_debug_params(?STATE_DYING, soldier_flash, 0, ShootEffectLocation, Params#params.direction, 0, [] ) } );
					  		_Else -> ok
					  end,
					ets:insert(?DBNAME, {Key, update_params(Params, Animation) })
	end.
%apdate params of unit woth next animation and frame delay
update_params(Params, Animation)->
	Params#params{frame_delay=update_frame_delay(Params#params.frame_delay),
				  img=get_next_image_num(Params#params.img, Params#params.frame_delay, length(Animation))}.


update_frame_delay(Current)->
	Next = Current - 1,
	case Next < 0 of
		true -> ?FRAME_DELAY;
		false -> Next
	end.
%update image num of next animation
get_next_image_num(Current, FrameDelay, ListLength) ->
	case FrameDelay of
		0 ->
			Next = (Current + 1) rem (ListLength + 1),
			case Next of
				0 -> Next + 1;
				_ -> Next
			end;
		_Else -> Current
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

% -record(gl_context, {canvas, tankImg, soldierImg, harvesterImg, mineImg, font, clockImg}).
init_gl_context(GL)->
	?DEBUG("Starting init gl_context~n",[]),
	ClockImg=load_texture_by_string("Simulation Time : " ++ [?PAD(0), $:, ?PAD(0)]),	
	Animations = init_animations(),
	
	?DEBUG("Finished init gl_context~n",[]),
	#gl_context{canvas=GL, animations = Animations, clockImg = ClockImg, 
				startTime=erlang:system_time(seconds)
				}.

create_debug_params(State, Type, Team, Location, Direction, SeeRange, Others) -> 
								  #params{state=State, img=1 ,type=Type, frame_delay=?FRAME_DELAY, team=Team, location=Location,
												   direction=Direction, see_range=SeeRange, others=Others}.

% Create all textures
init_animations()->
	?DEBUG("Init Animations~n",[]),
	#animations{explosion 		= [new_image(FileName) || FileName <- ?EXPLOSION],
				blue_tank		= [new_image(?TANK1, {30, 20})],
				red_tank		= [new_image(?TANK2, {30, 20})],
				red_harvester	= [new_image(?HARVESTER1, {30, 20})],
				blue_harvester	= [new_image(?HARVESTER2, {30, 20})],
				
				tank_fire		= [new_image(FileName, {30, 20}) || FileName <- ?TANK_SHOOT],
				soldier_fire	= [new_image(FileName, {15, 10}) || FileName <- ?SOLDIER_SHOOT],
				
				blue_soldier	= [new_image(FileName, {25, 20}) || FileName <- ?BLUE_SOLDIER],
				red_soldier		= [new_image(FileName, {25, 20}) || FileName <- ?RED_SOLDIER],
				soldier_dead	= [new_image(FileName, {35, 35}) || FileName <- ?SOLDIER_DEAD],
				blue_base 		= [new_image(FileName, {75, 75}) || FileName <- ?BLUE_BASE],
				red_base 		= [new_image(FileName, {75, 75}) || FileName <- ?RED_BASE],
				gold_coin 		= [new_image(FileName, {20, 20}) || FileName <- ?GOLD_COIN],
				silver_coin		= [new_image(FileName, {20, 20}) || FileName <- ?SILVER_COIN],
				mines 			= [new_image(FileName, {20, 20}) || FileName <- ?MINES],
 				backgrounds 	= [new_image(FileName) || FileName <- ?BACKGROUNDS],
				footprints		= [new_image(FileName, {15, 15}) || FileName <- ?FOOTPRINTS],
				tank_colis		= [new_image(FileName, {15, 15}) || FileName <- ?TANK_COLIS],
				collect_resource= [new_image(FileName) || FileName <- ?COLLECT_RESOURCE],
				victory			= [new_image(?VICTORY)],
				defeat			= [new_image(?DEFEAT)]
				}.

% create texture from string	
load_texture_by_string(String) ->
	Brush = wxBrush:new({0,0,0}),
	Font = wxFont:new(10, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
	Color = {40,40,40},
    TmpBmp = wxBitmap:new(200, 200),
    Tmp = wxMemoryDC:new(TmpBmp),
    wxMemoryDC:setFont(Tmp, Font),        
    {StrW, StrH} = wxDC:getTextExtent(Tmp, String),
    wxMemoryDC:destroy(Tmp),
    wxBitmap:destroy(TmpBmp),
    
    W = get_power_of_two_roof(StrW),
    H = get_power_of_two_roof(StrH),

    Bmp = wxBitmap:new(W, H),
    DC = wxMemoryDC:new(Bmp),
    wxMemoryDC:setFont(DC, Font),        
    wxMemoryDC:setBackground(DC, Brush),
	wxDC:setBackgroundMode( DC, ?wxTRANSPARENT ),
    wxMemoryDC:clear(DC),
    wxMemoryDC:setTextForeground(DC, {255, 255, 255}),
    wxMemoryDC:drawText(DC, String, {0, 0}),

    Img = wxBitmap:convertToImage(Bmp),
    
    Alpha = wxImage:getData(Img),
    Data = colourize_image(Alpha, Color),
    wxImage:destroy(Img),
    wxBitmap:destroy(Bmp),
    wxMemoryDC:destroy(DC),

    [TId] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, TId),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_RGBA, W, H, 0, ?GL_RGBA, ?GL_UNSIGNED_BYTE, Data),
	
    #img{glTextureID = TId, width = StrW, height = StrH, drawWidth = StrW / W, drawHeight = StrH / H}.

colourize_image(Alpha, {R,G,B}) ->
    << <<R:8,G:8,B:8,A:8>> || <<A:8,_:8,_:8>> <= Alpha >>.

get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).
get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X) -> get_power_of_two_roof_2(N*2, X).


new_image(FileName) -> new_image(FileName, none).
new_image(FileName, ScaleParams) ->
	case ScaleParams of
		none -> 
			WXImage = wxImage:new( FileName );
		{Width, Height} -> 
			WXImage = wxImage:scale(wxImage:new( FileName ), Width, Height)
	end,
    Img     = create_image( WXImage ),
    wxImage:destroy( WXImage ),
    Img.

% config gl 
init_gl(GL) ->
    ?DEBUG("Starting init gl~n",[]),
	{Width, Height} = wxWindow:getClientSize(GL),
    gl:viewport(0, 0, Width, Height),
   
	gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
   
    gl:enable(?GL_BLEND),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:blendFunc(?GL_ONE, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:ortho(0.0, Width, Height, 0.0, 0.0, 1.0),
 	gl:clearColor(255.0, 178.0, 102.0, 0.0),	
    gl:shadeModel(?GL_FLAT),  
	gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
	gl:loadIdentity(),

    gl:enable(?GL_TEXTURE_2D),
	gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    ?DEBUG("Finished init gl~n",[]),
    ok.


% convert wxImage to texture and create img record 
create_image( WXImage ) -> create_image( WXImage, no_mask ).
create_image( WXImage, MaskRGB ) ->
    Width       = wxImage:getWidth  ( WXImage  ),
    TexWidth    = get_power_of_two  ( Width    ),
    Height      = wxImage:getHeight ( WXImage  ),
    TexHeight   = get_power_of_two  ( Height   ),

    [GLTextureID] = gl:genTextures( 1 ),
    gl:bindTexture  ( ?GL_TEXTURE_2D, GLTextureID ),
    gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR ),
    gl:texParameteri( ?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR ),
	gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_REPLACE),


    HasAlpha = wxImage:hasAlpha( WXImage ),
    if
        HasAlpha ->
            PixelData = image_mix_data(
                    wxImage:getData(  WXImage ),
                    wxImage:getAlpha( WXImage ) ),
            GLNumBytes = ?GL_RGBA8,
            GLTexMode  = ?GL_RGBA,
            PixelBits  = 4;
        is_number( MaskRGB ) ->
            PixelData = image_mask_to_rgba( MaskRGB, wxImage:getData( WXImage ) ),
            GLNumBytes = ?GL_RGBA8,
            GLTexMode  = ?GL_RGBA,
            PixelBits  = 4;
        true ->
            PixelData = wxImage:getData( WXImage ),
            GLNumBytes = ?GL_RGB8,
            GLTexMode  = ?GL_RGB,
            PixelBits  = 3
    end,
    
    if
        (not (Width == TexWidth)) or (not (Height == TexHeight)) ->
            GLData = expand_data( PixelBits, PixelData, Width, Height, TexWidth, TexHeight );
        true ->
            GLData = PixelData
    end,
    
    gl:texImage2D( ?GL_TEXTURE_2D, 0, GLNumBytes, TexWidth, TexHeight, 0, GLTexMode, ?GL_UNSIGNED_BYTE, GLData    ),
    
    #img{
        width       = Width,
        height      = Height,
        drawWidth   = Width  / TexWidth,
        drawHeight  = Height / TexHeight,
        glTextureID = GLTextureID
    }.

%% @doc Returns the size of the image as it should appear on the screen.
%% @spec get_size(Image::image()) -> {Width::integer(), Height::integer()}
get_size(#img{width=Width, height=Height}) -> {Width, Height}.


expand_data(PixelBits, PixelData, Width, Height, TexWidth, TexHeight) ->
    add_row_data(
            TexWidth*PixelBits, TexHeight-Height,
            pad_data(
                    Width*PixelBits, (TexWidth-Width)*PixelBits,
                    PixelData, <<>>,
                    Height, Width*PixelBits )
    ).

pad_data(_Width, _WPadding, _OldPixels, NewPixels, 0, _W) ->
    NewPixels;
pad_data(Width, WPadding, OldPixels, <<NewPixels/binary>>, H, 0) ->
    pad_data(
            Width, WPadding,
            OldPixels, <<NewPixels/binary, 0:(WPadding*8)>>,
            H-1, Width );
pad_data(Width, WPadding, <<Bit:8, OldPixels/binary>>, <<NewPixels/binary>>, H, W) ->
    pad_data(
            Width, WPadding,
            OldPixels, <<NewPixels/binary, Bit:8>>,
            H  , W-1   ).

add_row_data( W, H, <<Pixels/binary>> ) -> <<Pixels/binary, 0:(W*H*8)>>.

image_mix_data( RGBData, AlphaData ) ->
    image_mix_data( RGBData, AlphaData, <<>> ).
image_mix_data( <<>>, <<>>, NewData ) ->
    NewData;
image_mix_data( <<RGB:24, RGBData/binary>>,
                <<A:8, AlphaData/binary>>,
                <<NewData/binary>> ) ->
    image_mix_data( RGBData, AlphaData, <<NewData/binary, RGB:24, A:8>> ).

image_mask_to_rgba(  MaskRGB, RGBData )       -> image_mask_to_rgba( MaskRGB, RGBData, <<>> ).
image_mask_to_rgba( _MaskRGB, <<>>, NewData ) -> NewData;
image_mask_to_rgba(  MaskRGB, <<RGB:24, RGBData/binary>>, <<NewData/binary>> ) ->
    if
        RGB =:= MaskRGB -> image_mask_to_rgba( MaskRGB, RGBData, <<NewData/binary, RGB:24,   0:8>> );
        true            -> image_mask_to_rgba( MaskRGB, RGBData, <<NewData/binary, RGB:24, 255:8>> )
    end.

get_power_of_two(Length) ->
    if
        (Length bor (Length-1) == 0) -> Length;
        true -> first_greater_power_of_two(Length)
    end.

first_greater_power_of_two(Length) -> first_greater_power_of_two(Length, 2).
first_greater_power_of_two(Length, PowerOfTwo) ->
    if
        (PowerOfTwo >= Length) -> PowerOfTwo;
        true -> first_greater_power_of_two(Length, PowerOfTwo*2)
    end.

destroy_image(Image) -> gl:deleteTextures([ Image#img.glTextureID ]).

%% @doc Draws the given image at the location given.
%%
%% The width and height allows the image to be re-scaled to that width and height.
%% The IsCentered states if the image should be drawn centred around the location
%% given, or not. So when IsCentered is true the X and Y is the centre of where
%% the image will be drawn. When IsCentered is false it is the top left-corner.
%%
%% @spec draw_image( G::graphics(), Img::image(), [X::float(), Y::float()], {Width::float(), Height::float()} ) -> ok
draw_image(Img, [X, Y])  ->
    draw_image_rotated(Img, [X + ?OFFSET, Y + ?OFFSET], get_size(Img), true, 0).

draw_image_rotated(Img, [X, Y], Degrees)  ->
	draw_image_rotated(Img, [X + ?OFFSET, Y + ?OFFSET], get_size(Img), true, Degrees).

draw_image_rotated(Img, [X, Y], {Width, Height}, IsCentered, Degrees ) ->
%%     Degrees = to_degrees( Angle ),
    gl:translatef( X, Y, 0.0 ),
    gl:rotatef( Degrees, 0.0, 0.0, 1.0 ),
    draw_image_inner( Img, [0, 0], {Width, Height}, IsCentered ),
    gl:rotatef( -Degrees, 0.0, 0.0, 1.0 ),
    gl:translatef( -X, -Y, 0.0 ).

to_radians(Degrees) -> Degrees * (math:pi() / 180.0).
%% @doc This is the real, internal image drawing code that does the work.
%% @spec draw_image_inner( Img::image(), {X::float(), Y::float()}, {Width::float(), Height::float()}, IsCentered::boolean() ) -> ok
draw_image_inner(Img, [X, Y], {Width, Height}, IsCentered) ->
    
	TexCoordX = Img#img.drawWidth,
    TexCoordY = Img#img.drawHeight,
	
%% 	NewY = (Y * ?CANVAS_HEIGHT) / ?WORLD_HEIGHT,
%% 	NewX = (X * ?CANVAS_WIDTH) / ?WORLD_WIDTH,
%% 	NewTexCoordY = (TexCoordY * ?CANVAS_HEIGHT) / ?WORLD_HEIGHT,
%% 	NewTexCoord = (TexCoordX * ?CANVAS_WIDTH) / ?WORLD_WIDTH,
	
    
    if
        IsCentered -> DrawX = X - Width/2, DrawY = Y - Height/2;
        true       -> DrawX = X          , DrawY = Y
    end,
 
	gl:texEnvf(?GL_TEXTURE_ENV, ?GL_TEXTURE_ENV_MODE, ?GL_MODULATE),

	gl:enable(?GL_TEXTURE_2D),
    gl:bindTexture(?GL_TEXTURE_2D, Img#img.glTextureID),
    gl:'begin'(?GL_QUADS),
    
    % bottom left
    gl:texCoord2d(0          , 0.0          ),
    gl:vertex2f(DrawX        , DrawY        ),
    % bottom right
    gl:texCoord2d(TexCoordX  , 0.0          ),
    gl:vertex2f(DrawX+Width  , DrawY        ),
    % top right
    gl:texCoord2d(TexCoordX  , TexCoordY    ),
    gl:vertex2f(DrawX+Width  , DrawY+Height ),
    % top left
    gl:texCoord2d(0          , TexCoordY    ),
    gl:vertex2f(DrawX        , DrawY+Height ),

    gl:'end'(),
    gl:disable(?GL_TEXTURE_2D).
