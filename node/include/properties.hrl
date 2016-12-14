
%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR


%%% NODE PROPERTIES
%-define(NETWORK_DEVICE, "enp2s0").
%-define(NETWORK_DEVICE, "wlp3s0").
-define(NETWORK_DEVICE, "lo").

-define(LOGGER, log).
-define(LOAD_NG, load_ng).
-define(LOAD_NG_CORE, load_ng_core).
-define(DATA_LINK, data_link).
-define(APPLICATION, simple_app).
-define(REPORT, report).
-define(NODE_RESOURCES, [?LOGGER, ?REPORT, ?LOAD_NG_CORE, ?DATA_LINK, ?LOAD_NG, ?APPLICATION]).

-define(NODE_PROPERTIES, node_properties).
-define(NODE_PROPS_LIST, [{protocol, ?LOAD_NG},
                          {node_name, test}
                         ]).




%%% hy-LOADng PROPERTIES
-define(PROTOCOL_PROPS_LIST, []).
-define(PROTOCOL_PROPERTIES, protocol_properties).

%%% REPORTING UNIT PROPERTIES



%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(PROTOCOL, load_ng).
-define(MESSAGE_SEND_INTERVAL, 5000). % 5 seconds interval between messages

-define(APPLICATION_PROPERTIES, application_properties).
-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
                         {role, smart_meter}
                        ]).
