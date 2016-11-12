-include("../include/defines.hrl").

%% Units Records

-record(params, {type,
				team,
				img,
				frame_delay,
				location, %% [X,Y]
				direction, %% 1-360
				see_range,
				others,
				state}).

-record(harvester_p, {hp,
					 gold,
					 iron,
					 target_x,
					 target_y,
					 target_pid,
					 local_server}).

-record(resource_p, {type,
					 amount,
					 local_server}).

-record(soldier_p,  {hp,
					 target_x,
					 target_y,
					 target_pid,
					 local_server}).

-record(tank_p,  {hp,
					 target_x,
					 target_y,
					 target_pid,
					 local_server}).

-record(base_p, {hp,
				 iron,
				 gold,
				 harv_num,
				 local_server}).

-define(GLOBAL_X, ?WORLD_WIDTH).
-define(GLOBAL_Y, ?WORLD_HEIGHT).
-define(DEAD_TIME, 40).
-define(BASE_OFFSET, 50).
-define(GO_TO_FIRE, 350).
-define(TICK, 50).