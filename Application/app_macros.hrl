%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2017 10:29
%%%-------------------------------------------------------------------
-author("liya").


% ================================ module_names ================================
% for external protocol use only.
-define(DC_MODULE, dc).
-define(AM_SEM_MODULE, am_sem).
-define(SM_SEM_MODULE, sm_sem).


% ================================ node_names =================================
% for external protocol use only.
-define(MASTER_NODE, 'master_node@cseadmin-ThinkStation-S30').
-define(DC_NODE, 'dc_node@cseadmin-ThinkStation-S30').
-define(SEM1_NODE, 'sem1_node@cseadmin-ThinkStation-S30').
-define(SEM_NODES, [?SEM1_NODE]).



% ================================= protocol_names =============================
% for external protocol use only.
-define(MASTER_PROT, master).
-define(DC_PROT, dc).
-define(SEM1_PROT, sem1).


%================================ properties ================================


-define(TIMEOUT, 20000).