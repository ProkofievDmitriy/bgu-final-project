%-define(MODULES_TO_LOG, ["load_ng_core", "report"]).
-define(MODULES_TO_LOG, [log,protocol_interface,report,load_ng_core,data_link,load_ng,modem_port,simple_app, node]).
%-define(MODULES_TO_LOG, [report,load_ng_core,data_link]).




%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % -1 - DEBUG+, 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR

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
-define(NODE_RESOURCES, [?LOGGER, ?PROTOCOL, ?REPORT_UNIT, ?LOAD_NG_CORE, ?DATA_LINK, ?LOAD_NG, ?MODEM_PORT, ?APPLICATION, stub_data_server]).


-define(NODE_PROPS_LIST, [{protocol, ?LOAD_NG}
%                          {node_name, node_1} % temp value , need to be define per each node.
                         ]).




%%% hy-LOADng PROPERTIES
-define(ADDRESS_LENGTH, 5). % number of bits to store address
-define(NET_TRAVERSAL_TIME, 2000).


-define(LOAD_NG_CORE_PROP_LIST, [{address_length, ?ADDRESS_LENGTH},
                                 {net_traversal_time, ?NET_TRAVERSAL_TIME},
                                 {reporting_unit, ?REPORT_UNIT}
                                ]).
-define(DATA_LINK_PROPS_LIST, []).

-define(PROTOCOL_PROPS_LIST, [{?LOAD_NG_CORE_PROPERTIES, ?LOAD_NG_CORE_PROP_LIST},
                              {?DATA_LINK_PROPERTIES, ?DATA_LINK_PROPS_LIST}
                             ]).

%%% REPORTING UNIT PROPERTIES
-define(REPORT_UNIT_PROPS_LIST, [{data_server_name,stub_data_server},
                                 {data_server_ip, "192.168.14.98"}
%                                 {data_server_ip, "127.0.0.1"}
                                ]).


%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(MESSAGE_SEND_INTERVAL, 15000). % 15 seconds interval between messages

-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
                         {role, smart_meter}
                        ]).
