%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%       =================== under construction =====================
%%% @end
%%% Created : 01. Aug 2017 11:06
%%%-------------------------------------------------------------------
-module(dc_app).
-author("liya").

-behaviour(gen_fsm).

-include("app_macros.hrl").

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  discovering/2,
  handle_event/3,
  handle_info/3,
  terminate/3,
  code_change/4]).



-record(state, {my_pid,                % app name
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
                ter8,
                sn,
                timer}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({MyNode, MyProtocol,ReportingUnit, Meters}) ->
  Me = erlang:list_to_atom(atom_to_list(MyNode)++"_app"),
  log:info("[~p]  ~p created ~n",[?MODULE,Me]),
  {ok,Pid}=gen_fsm:start({local,Me}, ?MODULE, {Me,MyProtocol,MyNode,ReportingUnit,Meters},[]),
  Pid.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init({Me, MyNode, MyProtocol,ReportingUnit, Meters}) ->
  Hand_shake =hand_shake(Me,MyProtocol,1),
  case Hand_shake of
    ready ->
      log:info("[~p]  ~p initialized~n", [?MODULE,Me]),
      Fd = app_utils:open_report_file(Me),
      _ = app_utils:create_and_initialize_sets(Meters),
      Timerpid = erlang:spawn(app_utils, timer, [Me,?DISCOVERING_TIMEOUT]),
      State = #state{
           my_pid = Me,
           my_node = MyNode,
           my_protocol = MyProtocol,
           reporting_unit = ReportingUnit,
           reporting_file = Fd,
           meters = Meters,     %TODO maybe ?METERS
           mediums = ?MEDIUMS,
           exp_counter = 0,
           sn =0,
           timer = Timerpid
      },
      _ = app_utils:report_start_of_experiment(State),
      {ok, discovering, State};

    {terminate, Reason} -> log:critical("[~p]  handshake with ~p failed with message:
         ~p~n", [?MODULE,MyProtocol,Reason]),
      {stop,{handshake_failure,Reason}}
end.

discovering(timeout,State) when State#state.sn==0 ->
  Rd = app_utils:random_elements(State#state.meters),
  Nrs = app_utils:delete_elements(State#state.meters,Rd),
  _ = app_utils:send_dreq(State#state.my_protocol,Rd,1),
  State#state{rd = Rd,
              nrs = Nrs,
              ter = [],
              ter8 = []},
  _ = app_utils:report_sending_dreq(State)
  log:info("[~p]  first dreq sent, Rd are ~p~n",[?MODULE,Rd]),



handle_event({received_message, Bit_string}, StateName, State) ->
  log:info("[~p]  received bit string: ~p in state: ~p~n", [?MODULE,Bit_string,StateName]),
  <<Type:1, To_n:?NODE_BITS, Seq:?SEQ_BITS, Data_b/bitstring>> = Bit_string,
  To = utils:get_node_name(To_n),
  case Type of
    ?DREQ_BIT -> gen_fsm:send_event(State#state.my_pid, {dreq,To,Seq}),
      {next_state, StateName, State};
    ?DREP_BIT ->
      Data_size = bit_size(Data_b),
      if Data_size rem ?ENTRY_SIZE =/= 0 ->
        log:err("[~p]  received invalid data of size ~p, dropping drep~n",[?MODULE,Data_size]),
        {next_state, StateName, State};
        true ->


  {next_state, StateName, State}.




handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

