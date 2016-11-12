%Modules names define
-define(GRAPHICS_SERVER, t).
-define(DATA_SERVER, g).
-define(TIMER_INTERVAL, 40).
-define(SPAWN_RES_MINES, 2000).
-define(SERVER_NAMES, [bl, br, tl, tr]).
-define(LOCAL_SERVER_ADDRESSES, ['bl@127.0.0.1','br@127.0.0.1','tl@127.0.0.1','tr@127.0.0.1']).
-define(WORLD_WIDTH , 1200).
-define(WORLD_HEIGHT , 600).
-define(WORLD_ZERO_WIDTH, 0).
-define(WORLD_ZERO_HEIGHT, 0).
-define(WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT div 2).
-define(WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH div 2).
-define(GETX(List), lists:nth(1, List)).
-define(GETY(List), lists:nth(2, List)).

-define(LIST_OF_NODES,[{tl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]},
					   {tr,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_HEIGHT_MIDDLE, ?WORLD_HEIGHT]},
					   {bl,[?WORLD_ZERO_WIDTH, ?WORLD_WIDTH_MIDDLE],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]},
					   {br,[?WORLD_WIDTH_MIDDLE, ?WORLD_WIDTH],[?WORLD_ZERO_HEIGHT, ?WORLD_HEIGHT_MIDDLE]}]).



-define(STATE_AWAIT, await).
-define(STATE_SEEK, seek).
-define(STATE_ATTACK, attack).
-define(STATE_GO_TO_TARGET, go_to_target).
-define(STATE_DYING, dead).
-define(STATE_FIND_ENEMIES, findEnemies).
 
-define(BLUE_TEAM, 1).
-define(BLUE_TEAM_LABEL, "Blue Team").
-define(RED_TEAM, 2).
-define(RED_TEAM_LABEL, "Red Team").

-define(MOOVING, 0).
-define(SHOOTING, 1).
-define(DEAD, 3).



%Data Server
-define(DBNAME, etsDB).
-define(LOCAL_STORAGE, localSharedEts).
-define(INTERNALSTORAGE, intStor).

-record (gcontext ,{paint_func, img}).

%% graphic_server.erl
% macros
-define(DEBUG(Format, Args), io:format("**debug** " ++ Format, Args)).
-define(PAD4, fun(Int) -> string:right(integer_to_list(trunc(Int)), 4, $0) end).
-define(PAD, fun(Int) -> string:right(integer_to_list(trunc(Int)), 2, $0) end).

% constans
-define(CANVAS_HEIGHT , ?WORLD_HEIGHT + 30).
-define(CANVAS_WIDTH , ?WORLD_WIDTH + 30).
-define(WINDOW_HEIGHT , ?CANVAS_HEIGHT + 90).
-define(WINDOW_WIDTH , ?CANVAS_WIDTH + 50).

-define(OFFSET, 15).

%stat
-record(team_stat, {gold, iron, harvesters, soldiers, tanks, units_killed}).
-define(RED_TEAM_STAT_ID, 123).
-define(BLUE_TEAM_STAT_ID, 312).
-define(STAT_FORMAT, fun(Stat = #team_stat{})->
							"Gold : " ++ ?PAD4(Stat#team_stat.gold) ++ "		             " ++ 
							"Iron : " ++ ?PAD4(Stat#team_stat.iron) ++ "\n" ++ 
							"Harvesters : " ++ ?PAD4(Stat#team_stat.harvesters) ++ "		  " ++ 
  							"Soldiers : " ++ ?PAD4(Stat#team_stat.soldiers) ++ "\n" ++ 
							"Tanks : " ++ ?PAD4(Stat#team_stat.tanks) ++ "		            " ++ 
							"Units Dead : " ++ ?PAD4(Stat#team_stat.units_killed) end).
%images
-define(TANK_FILE , "../img/tank1.png").

%Buttons
-define(B1_LABEL, "Tank").
-define(B2_LABEL, "Soldier").
-define(B3_LABEL, "Mine").
-define(B4_LABEL, "Resource").
-define(B5_LABEL, "Harvester").
-define(B6_LABEL, "Kill All").
-define(B6_ID, 6).
-define(B7_LABEL, "Start Game").
-define(B7_ID, 7).

-record(gl_context, {canvas, 
						animations,
						font, 
						clockImg, 
						startTime}).

-record(animations, {backgrounds, explosion, red_harvester, blue_harvester, 
					 tank_colis, mines, red_tank, blue_tank, blue_soldier, red_soldier, gold_coin, silver_coin, footprints,
  					tank_fire, soldier_fire, blue_base, red_base, status_bar, collect_resource, soldier_dead, backgroud_locations, victory, defeat, sand}).
%explosion png
-define(FRAME_DELAY, 1).
-define(TANK1,"../img/blue_tank/tank1.png").
-define(TANK2,"../img/red_tank/tank2.png").
-define(HARVESTER1,"../img/harvester/1.png").
-define(HARVESTER2,"../img/harvester/2.png").

-define(COLLECT_RESOURCE, ["../img/collect_resource/1.png",
						   "../img/collect_resource/2.png",
						   "../img/collect_resource/3.png",
						   "../img/collect_resource/4.png",
						   "../img/collect_resource/5.png",
						   "../img/collect_resource/6.png",
						   "../img/collect_resource/7.png",
						   "../img/collect_resource/8.png",
						   "../img/collect_resource/9.png",
						   "../img/collect_resource/10.png",
						   "../img/collect_resource/11.png",
						   "../img/collect_resource/12.png",
						   "../img/collect_resource/13.png"
							]).

-define(TANK_SHOOT, ["../img/blue_tank/attack4.png",
					 "../img/blue_tank/attack3.png",
					 "../img/blue_tank/attack3.png",
					 "../img/blue_tank/attack2.png",
					 "../img/blue_tank/attack2.png",
					 "../img/blue_tank/attack.png",
					 "../img/blue_tank/attack.png"
					]).

-define(SOLDIER_SHOOT, ["../img/blue_soldier/fire3.png",
						"../img/blue_soldier/fire3.png",
						"../img/blue_soldier/fire2.png",
						"../img/blue_soldier/fire2.png",
						"../img/blue_soldier/fire.png",
						"../img/blue_soldier/fire.png"
					   ]).

-define(VICTORY, ["../img/victory.png"]).
-define(DEFEAT, ["../img/defeat.png"]).
-define(SAND, ["../img/desert.jpg"]).

-define(GOLD_COIN,["../img/gold_coin/7.png",
				   "../img/gold_coin/2.png",
				   "../img/gold_coin/3.png",
				   "../img/gold_coin/4.png",
				   "../img/gold_coin/5.png",
				   "../img/gold_coin/6.png",
				   "../img/gold_coin/7.png"
				   ]).
-define(SILVER_COIN,["../img/silver_coin/1.png",
					 "../img/silver_coin/2.png",
					 "../img/silver_coin/3.png",
					 "../img/silver_coin/4.png",
					 "../img/silver_coin/5.png",
					 "../img/silver_coin/6.png",
					 "../img/silver_coin/1.png"
]).


-define(FOOTPRINTS,["../img/footprints/1.png",
					"../img/footprints/1.png",
					"../img/footprints/1.png"
				   ]).

			   
-define(MINES,["../img/mines/3.png",
				   "../img/mines/3.png",
				   "../img/mines/3.png",
				   "../img/mines/3.png",
				   "../img/mines/1.png",
				   "../img/mines/1.png",
				   "../img/mines/2.png",
				   "../img/mines/2.png",
				   "../img/mines/1.png",
				   "../img/mines/1.png",
				   "../img/mines/1.png",
				   "../img/mines/1.png",
				   "../img/mines/2.png",
				   "../img/mines/1.png"
				  ]).				   
			   
-define(SOLDIER_DEAD,["../img/soldier_dead/1.png",
				   "../img/soldier_dead/2.png",
				   "../img/soldier_dead/3.png",
				   "../img/soldier_dead/4.png",
				   "../img/soldier_dead/5.png",
				   "../img/soldier_dead/6.png",
				   "../img/soldier_dead/7.png",
				   "../img/soldier_dead/8.png"
				  ]).				   
				   
				   
-define(EXPLOSION,["../img/explosion/0.png",
				   "../img/explosion/1.png",
				  "../img/explosion/2.png",
				  "../img/explosion/3.png",
				  "../img/explosion/4.png",
				  "../img/explosion/5.png",
				  "../img/explosion/6.png",
				  "../img/explosion/7.png",
				  "../img/explosion/8.png",
				  "../img/explosion/9.png",
				  "../img/explosion/10.png",
				  "../img/explosion/11.png",
				  "../img/explosion/12.png",
				  "../img/explosion/13.png",
				  "../img/explosion/14.png",
				  "../img/explosion/15.png"
		 ]).


-define(BLUE_SOLDIER,[
					  "../img/blue_soldier/2.png",
					  "../img/blue_soldier/3.png",
					  "../img/blue_soldier/4.png",
					  "../img/blue_soldier/5.png",
					  "../img/blue_soldier/4.png",
					  "../img/blue_soldier/3.png",
					  "../img/blue_soldier/2.png",
					  "../img/blue_soldier/6.png",
					  "../img/blue_soldier/7.png",
					  "../img/blue_soldier/8.png",
					  "../img/blue_soldier/9.png",
					  "../img/blue_soldier/8.png",
					  "../img/blue_soldier/7.png",
					  "../img/blue_soldier/6.png"
					 ]).


-define(RED_SOLDIER,["../img/red_soldier/1.png",
					  "../img/red_soldier/2.png",
					  "../img/red_soldier/3.png",
					  "../img/red_soldier/4.png",
					  "../img/red_soldier/5.png",
					  "../img/red_soldier/4.png",
					  "../img/red_soldier/3.png",
					  "../img/red_soldier/2.png",
					  "../img/red_soldier/1.png",
					  "../img/red_soldier/6.png",
					  "../img/red_soldier/7.png",
					  "../img/red_soldier/8.png",
					  "../img/red_soldier/7.png",
					  "../img/red_soldier/6.png"
					 ]).


-define(TANK_COLIS,["../img/tank_colis/11.png", 
					"../img/tank_colis/12.png", 
					"../img/tank_colis/13.png", 
					"../img/tank_colis/14.png", 
					"../img/tank_colis/15.png" 
					]).

-define(BACKGROUNDS, ["../img/bc1.png",
					  "../img/bc2.png",
					  "../img/bc4.png",
					  "../img/hit.png",
					  "../img/bc5.png",
					  "../img/bc6.png",
					  "../img/decal.png",
					  "../img/hit.png",
					  "../img/bc3.png"]).

-define(BACKGROUD_LOCATIONS,[[25,25],[57,145],[120,567],
							 [256,345],[305,120],[923,245],
							 [345,25],[413,25],[654,25],
							 [1064,25],[750,300],[527,550],
							 [40,290],[490,320],[935,470],
							 [846,200],[237,450],[812,560],
							 [50,590],[1200,350],[1190,560],
							 [678,720],[210,270],[978,321],
							 [1123,632],[443,300],[1200,534]]).

-define(BLUE_BASE,["../img/base/blue/0.png",	
				  "../img/base/blue/1.png",
				  "../img/base/blue/2.png",
				  "../img/base/blue/3.png",
				  "../img/base/blue/4.png",
				  "../img/base/blue/5.png",
				  "../img/base/blue/6.png",
				  "../img/base/blue/7.png",
				  "../img/base/blue/8.png",
				  "../img/base/blue/9.png",
				  "../img/base/blue/10.png",
				  "../img/base/blue/11.png"
				  ]).

-define(RED_BASE,["../img/base/red/0.png",
				  "../img/base/red/1.png",
				  "../img/base/red/2.png",
				  "../img/base/red/3.png",
				  "../img/base/red/4.png",
				  "../img/base/red/5.png",
				  "../img/base/red/6.png",
				  "../img/base/red/7.png",
				  "../img/base/red/8.png",
				  "../img/base/red/9.png",
				  "../img/base/red/10.png",
				  "../img/base/red/11.png"
				  ]).

