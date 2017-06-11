-module(application_interface).


-define(PROTOCOL_NAME, protocol).
-define(NODE_NAME, node_name).
-define(APPLICATION_MODULE, application_module) % might be: dc/ am_sem/ sm_sem

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([start_application/1, handle_call/3, rise_dreq_to_app/2, rise_drep_to_app/2, ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   API Functions Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_application(Meters_list) ->
   spawn(?APPLICATION_MODULE, start_link,[?NODE_NAME,?PROTOCOL_NAME, Meters_list]).

% handle_call({app_handshake, ApplicationName, App_role}, ApplicationName, State) ->
%  {reply, ok, State};
%
%  handle_call({dreq, Destination, Seq_num}, ApplicationName,State) ->
% 	send_the_dreq_to_its_destination(),
% 	{reply,ok,State};
%
%
% handle_call ({drep, Destination, Data, Seq_num}, ApplicationName, State) ->
% 	send_the_drep_to_its_destination(),
% 	{reply,pk,State}.

rise_message(ApplicationName, Data)->
    gen_fsm:send_event(ApplicationName, Data).


% rise_message(ApplicationName, Data)->
%     gen_fsm:send_event(ApplicationName, {received_message, Data}).
% rise_dreq_to_app({dreq, Destination, Seq_num}, ApplicationName) -> % use only if Destination == ?NODE_NAME
% 	gen_fsm:send_event(ApplicationName, {dreq, Destination, Seq_num}).
%
% rise_drep_to_app({drep, Destination, Data, Seq_num},ApplicationName) ->
% 	gen_fsm:send_event(ApplicationName, {drep, Destination, Data, Seq_num}).
