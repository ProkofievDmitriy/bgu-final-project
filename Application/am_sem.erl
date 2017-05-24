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
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1,
  counting/2,
  handle_info/3,
  terminate/3,
  code_change/4]).


%% TODO: discuss initialization with dima - he should send me my name?


%%%===================================================================
%%% API
%%%===================================================================

start_link({My_name, My_server}) ->
  io:format("~p created~n",[My_name]),
  gen_fsm:start_link({local, My_name}, ?MODULE,{My_name,My_server}, []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================


%% My_name - self
%% My_server - the gen server of the protocol in the same node
init({My_name,My_server}) ->
  io:format("~p initialized~n", [My_name]),
  {ok, counting, {My_name,My_server,0,0}}.


%%TODO - trigger increment event
counting(increment, {My_name,My_server,Counter,Sn}) ->
  io:format("~p is icrementing, last reading: ~p~n",[My_name,Counter]),
  {next_state, counting, {My_name,My_server,Counter+1,Sn}};

%%TODO - consider replacing with 2 function using pattern matching on To(Me/other)
counting({dreq,To,Seq},{My_name,My_server,Counter,Sn}) ->
  io:format("~p received dreq with Sn ~p~n", [My_name,Seq] ),
  %% if the dreq was meant to me
  if To == My_name ->
    %% if the dreq has a bigger sequence number - send reading and update my sn
    if Sn<Seq ->
      %%TODO replace message with call/cast/event
      %% sending reading
      io:format("~p is sending reading ~n", [My_name] ),
      My_server ! {drep,[{My_name,Counter}], Seq},
      %% returning to the same state with updated sequence number
      {next_state, counting, {My_name,My_server,Counter,Seq}};
    %% if seq lower or equals - ignore
      true ->
        io:format("~p is ignoring dreq ~n", [My_name] ),
        {next_state, counting, {My_name,My_server,Counter,Sn}}
    end;
    %% if the dreq was not meant to me pass it back
    true ->
      %%TODO replace message with call/cast/event
      io:format("~p is passing a dreq ~p~n", [My_name,Seq] ),
      My_server ! {dreq,To,Seq},
      {next_state, counting, {My_name,My_server,Counter,Sn}}
  end;

counting({drep,Data,Seq},{My_name,My_server,Counter,Sn}) ->
  io:format("~p received drep with Sn ~p~n", [My_name,Seq] ),
  %% if the drep has a newer sequence number- append my reading
  if Sn<Seq ->
    io:format("~p is appending reading ~n", [My_name] ),
    %%TODO replace message with call/cast/event
    My_server ! {drep,[{My_name,Counter}|Data],Seq},
    %% returning to the same state with updated sequence number
    {next_state, counting, {My_name,My_server,Counter,Seq}};
  %% if seq lower or equals - ignore
    true ->
      io:format("~p is passing drep ~n", [My_name] ),
      My_server ! {drep,[Data],Seq},
      {next_state, counting, {My_name,My_server,Counter,Sn}}
  end;

counting(Event,{My_name,My_server,Counter,Sn}) ->
  io:format("~p recaived UNEXPECTED EVENT ~p~n", [My_name,Event] ),
  {next_state, counting, {My_name,My_server,Counter,Sn}}.



handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
