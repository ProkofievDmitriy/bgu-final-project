%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2017 10:29
%%%----------------------------------------------------------------------
-author("liya").

-record(state, {
  my_pid,                % app name
  my_node,               % pid of protocol process in the same node
  my_protocol,           % pid of node process
  reporting_unit,        % report module
  reporting_file,        % file descriptor of backup report file
  meters,                % list of current active nodes
  mediums,               % list of current active nodes and their mediums
  exp_counter,           % current experiment iteration
  nrs,
  rd,
  ter,
  sn,
  timer,
  term_times}).


-record(sem_state,{
  my_pid,
  my_node,
  my_protocol,
  reporting_unit,
  reporting_file,
  count
}).


% ========================== experiment properties========================
-define(TEST_MODE, integrated). % local - no OTP behavior | integrated - with OTP behavior
-define(EXP_MODE, gui). %% hardcoded / gui
-define(EXP_ID, "1").
-define(METERS, [node_4,node_6,node_9,node_10]).
-define(MEDIUMS,[{node_1, plc_only},{node_6,dual},{node_4,dual},{node_9,dual},{node_10,dual}]).
-define(AMR_MODE, am).
-define(PHASE1_COUNT, 1).
-define(PHASE2_COUNT, 0).
-define(EXP_COUNT, 1).


% ================================ macros ================================
% for external protocol use only.
-define(DC_MODULE, dc).
-define(SEM_MODULE, am_sem).
-define(DC_NODE, node_1).


%======================= bit string macros ===============================
-define(DREQ_BIT, 0).
-define(DREP_BIT, 1).
-define(NODE_BITS,7).
-define(READING_BITS, 17).
-define(SEQ_BITS,8).
-define(ENTRY_SIZE, ?NODE_BITS+?READING_BITS).




%================================ properties ================================


%%-define(DISCOVERING_TIMEOUT,30000).
%%-define(COLLECTING_TIMEOUT,30000).


-define(BETWEEN_EXP_TIMEOUT,30000).
-define(INITIALIZATION_TIMEOUT, 10000).
-define(DISCOVERING_TIMEOUT,10000).
-define(COLLECTING_TIMEOUT,30000).



-define(HAND_SHAKE_MAX_TRIES,5).
-define(HAND_SHAKE_TIMEOUT, 300000).
-define(TERMINATION_TIMEOUT, 3000).
-define(SM_WAITING_TIME, 10).

-define(MAX_DREQ_TRIES, 6).
-define(EXTRA_DISCOVERY_TRIES,15).
-define(MAX_TERMINALS_TIMES, 3).


-define(PROTOCOL_REQUEST_TIMEOUT, 10000).
