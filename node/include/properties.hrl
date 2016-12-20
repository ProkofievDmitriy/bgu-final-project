
%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR

-define(LOGGER, log).
-define(LOAD_NG, load_ng).
-define(LOAD_NG_CORE, load_ng_core).
-define(DATA_LINK, data_link).
-define(APPLICATION, simple_app).
-define(REPORT_UNIT, report).
-define(MODEM_PORT, modem_port).
-define(PROTOCOL, protocol_interface).

%%% NODE PROPERTIES
%-define(NETWORK_DEVICE, "enp2s0").
%-define(NETWORK_DEVICE, "wlp3s0").
-define(NETWORK_DEVICE, "lo").
-define(NODE_RESOURCES, [?LOGGER, ?PROTOCOL, ?REPORT_UNIT, ?LOAD_NG_CORE, ?DATA_LINK, ?LOAD_NG, ?MODEM_PORT, ?APPLICATION]).

-define(NODE_PROPERTIES, node_properties).
-define(NODE_PROPS_LIST, [{protocol, ?LOAD_NG},
                          {node_name, node_1}
                         ]).




%%% hy-LOADng PROPERTIES
-define(ADDRESS_LENGTH, 5). % number of bits to store address
-define(PROTOCOL_PROPS_LIST, [{address_length, ?ADDRESS_LENGTH}]).
-define(PROTOCOL_PROPERTIES, protocol_properties).

%%% REPORTING UNIT PROPERTIES
-define(REPORT_UNIT_PROPS_LIST, []).
-define(REPORT_UNIT_PROPERTIES, report_unit_properties).


%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(MESSAGE_SEND_INTERVAL, 15000). % 15 seconds interval between messages

-define(APPLICATION_PROPERTIES, application_properties).
-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
                         {role, smart_meter}
                        ]).
