%%%-------------------------------------------------------------------
%%% @author liya
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Mar 2017 10:18
%%%-------------------------------------------------------------------
-module('am_sem').
-author("liya").

-behaviour(gen_fsm).
-include("app_macros.hrl").

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
  counting/2,
  handle_info/3,
  terminate/3,
  code_change/4]).


%% TODO: discuss initialization with dima - he should send me my name?

%%TODO: add TO in drep format.

%% TODO add a handshake before doing any activity (gen_server:call)

%%%===================================================================
%%% API
%%%===================================================================

start_link(My_name, My_server,_Meters) ->
  log:info("~p created ~n",[My_name]),
  gen_fsm:start_link({local, My_name}, ?MODULE,{My_name,My_server}, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================


%% My_name - self
%% My_server - the gen server of the protocol in the same node
init({My_name,My_server}) ->
  Hand_shake =hand_shake(My_name,My_server,1),
  case Hand_shake of
   ready ->
    log:info("~p initialized~n", [My_name]),
     {ok, counting, {My_name,My_server,0,0}};
    {terminate, Reason} ->
      log:critical("handshake with ~p failed with message: ~p~n", [My_server,Reason]),
      {stop,{handshake_failure,Reason}}
  end.


%%TODO - trigger increment event
counting(increment, {My_name,My_server,Counter,Sn}) ->
  io:format("~p is icrementing, last reading: ~p~n",[My_name,Counter]),
  {next_state, counting, {My_name,My_server,Counter+1,Sn}};

%%TODO - consider replacing with 2 function using pattern matching on To(Me/other)
counting({dreq,To,Seq},{My_name,My_server,Counter,Sn}) ->
  log:debug("~p received dreq with Sn ~p~n", [My_name,Seq] ),
  %% if the dreq was meant to me
  if To == My_name ->
    %% if the dreq has a bigger sequence number - send reading and update my sn
    if Sn<Seq ->
      %% sending reading
      log:info("~p is sending reading ~n", [My_name] ),
      _Ok = send_drep (My_server,[{My_name,Counter}|[]],Seq),
      %% returning to the same state with updated sequence number
      {next_state, counting, {My_name,My_server,Counter,Seq}};
    %% if seq lower or equals - ignore
      true ->
        log:debug("~p received dreq ,with Seq ~p, local Sn ~p, ignoring~n", [Seq,Sn,My_name] ),
        {next_state, counting, {My_name,My_server,Counter,Sn}}
    end;
    %% if the dreq was not meant to me pass it back
    true ->
      log:err("~p received dreq with destination missmatch, passing on to ~p~n", [My_name,To] ),
      _Ok = send_dreq(My_server, To, Seq),
      {next_state, counting, {My_name,My_server,Counter,Sn}}
  end;

counting({drep,To,Data,Seq},{My_name,My_server,Counter,Sn}) ->
  case To of
    ?DC_NODE ->
  log:info("~p received drep with Sn ~p~n", [My_name,Seq] ),
  %% if the drep has a newer sequence number- append my reading
  if Sn<Seq ->
    log:debug("~p is appending reading ~n", [My_name] ),
    _Ok = send_drep(My_server, [{My_name,Counter}|Data],Seq),
    %% returning to the same state with updated sequence number
    {next_state, counting, {My_name,My_server,Counter,Seq}};
  %% if seq lower or equals - ignore
    true ->
      log:err("~p received drep with Seq ~p, local Sn ~p. passing drep ~n", [My_name,Seq,Sn]),
      _Ok = send_drep(My_server,Data, Seq),
      {next_state, counting, {My_name,My_server,Counter,Sn}}
      end;
    Dest ->
      log:err("~p received drep with wrong dest address of: ~p, ignoring~n",[My_name, Dest]),
      {next_state, counting, {My_name,My_server,Counter,Sn}}
  end;

counting(Event,{My_name,My_server,Counter,Sn}) ->
  log:err("~p recaived UNEXPECTED EVENT ~p~n", [My_name,Event] ),
  {next_state, counting, {My_name,My_server,Counter,Sn}}.



handle_info(Info, StateName, State) ->
  log:err(" ~p received UNEXPECTED MESSAGE ~p in state ~ with data ~p",[self(),Info,StateName,State]),
    {next_state, StateName, State}.

terminate(Reason, StateName, State) ->
  log:info("terminating with info: reason : ~p, state: ~p,~n state data: ~p~n",
    [Reason,StateName,State]),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hand_shake(Me,My_server,Times) ->
  case ?TEST_MODE of
    local ->
      My_server! {app_handshake, {Me,dc}},  %% format: {pid/name , type(dc/sem)}
      receive
        ok -> ready;
        Err-> case Times of
                Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                  log:err("handshake failed with err ~p, on try number: ~p , trying again~n",[Err,Times]),
                  hand_shake(Me,My_server,Times+1);
                Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                  log:err("handshake failed with err ~p, on try number: ~p , TERMINATING~n",[Err,Times]),
                  {terminate, Err}
              end
      after ?HAND_SHAKE_TIMEOUT -> case Times of
                                     Times when Times< ?HAND_SHAKE_MAX_TRIES ->
                                       log:err("handshake timeout on try number: ~p , trying again~n",[Times]),
                                       hand_shake(Me,My_server,Times+1);
                                     Times when Times >= ?HAND_SHAKE_MAX_TRIES ->
                                       log:err("handshake timeout on try number: ~p , TERMINATING~n",[Times]),
                                       {terminate, timeout}
                                   end
      end;
    integrated -> ok
    %% translate local to OTP
  end.

send_drep(My_server,Data,Seq) ->
  case ?TEST_MODE of
    local ->
      My_server ! {drep,?DC_NODE,Data,Seq},
      ok;
    integrated ->
      % translate local to OTP.
      ok
 end .

send_dreq(My_server, To, Seq) ->
  case ?TEST_MODE of
    local ->
      My_server! {dreq, To,Seq},
      ok;
    integrated ->
      %translate local to OTP.
  ok
  end.