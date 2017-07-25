-module(layer_interface).
 
-callback send(CurrentPid :: pid(), Destination :: any(), Message :: any()) -> 'ok' | {ok, sent} | {error, any()}.
-callback send_async(CurrentPid :: pid(), Destination :: any(), Message :: any()) -> 'ok' | {ok, sent} | {error, any()}.

-callback handle_incoming_message(CurrentPid :: pid(), Message :: binary()) -> 'ok'.
 
-callback updateUpperLevel(CurrentPid :: pid(), BottomLevelModule :: module(), BottomLevelPid :: pid()) -> 'ok'.
-callback updateBottomLevel(CurrentPid :: pid(), UpperLevelModule :: module(), UpperLevelPid :: pid()) -> 'ok'.
