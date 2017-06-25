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

-define(DISCOVERING_TIMEOUT,10000).
-define(COLLECTING_TIMEOUT,30000).




-define(HAND_SHAKE_MAX_TRIES,5).
-define(HAND_SHAKE_TIMEOUT, 3000).
-define(TERMINATION_TIMEOUT, 3000).
-define(SM_WAITING_TIME, 10).

-define(MAX_DREQ_TRIES, 6).
-define(EXTRA_DISCOVERY_TRIES,15).
-define(MAX_TERMINALS_TIMES, 3).


-define(PROTOCOL_REQUEST_TIMEOUT, 10000).




-define(DREQ_BIT, 0).
-define(DREP_BIT, 1).
-define(NODE_BITS,7).
-define(READING_BITS, 17).
-define(SEQ_BITS,8).
