%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2017 10:29
%%%-------------------------------------------------------------------
-author("liya").


% ================================ macros ================================
% for external protocol use only.
-define(DC_MODULE, dc).
-define(SEM_MODULE, am_sem).

-define(DC_NODE, node_1).






%================================ properties ================================


-define(TEST_MODE, integrated). % local - no OTP behavior | integrated - with OTP behavior

-define(TIMER_TIMEOUT, 18000).
-define(HAND_SHAKE_MAX_TRIES,3).
-define(HAND_SHAKE_TIMEOUT, 3000).
-define(TERMINATION_TIMEOUT, 3000).

-define(PROTOCOL_REQUEST_TIMEOUT, 10000).
