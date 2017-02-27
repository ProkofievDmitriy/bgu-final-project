%-define(MODULES_TO_FILTER, [modem_port]).
-define(MODULES_TO_FILTER, []).




%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % -1 - DEBUG+, 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR

-define(LOGGER, log).
-define(LOAD_NG, load_ng).
-define(NETWORK, load_ng_core).
-define(DATA_LINK, data_link).
-define(APPLICATION, simple_app).
-define(REPORT_UNIT, report).
-define(MODEM_PORT, modem_port).
-define(PROTOCOL, protocol_interface).
-define(TRANSPORT, transport).

%%% NODE PROPERTIES
%-define(NETWORK_DEVICE, "enp2s0").
%-define(NETWORK_DEVICE, "wlp3s0").
-define(NETWORK_DEVICE, "lo").
-define(NODE_RESOURCES, [?LOGGER, ?PROTOCOL, ?REPORT_UNIT, ?NETWORK, ?DATA_LINK, ?TRANSPORT, ?LOAD_NG, ?MODEM_PORT, ?APPLICATION, stub_data_server]).


-define(NODE_PROPS_LIST, [{protocol, ?LOAD_NG}
%                          {node_name, node_1} % temp value , need to be define per each node.
                         ]).




%%% hy-LOADng PROPERTIES
-define(BROADCAST_ADDRESS, 0).

%TODO Address length and message type currently should give 2 bytes for correct working (crc32 in modem port calculation) - integer number of bytes (not bitstring), should be fixed
-define(ADDRESS_LENGTH, 7). % number of bits to store address
-define(MESSAGE_TYPE_LENGTH, 3). % number of bits to store address
-define(SESSION_MANAGEMENT_LENGTH, 8). % number of bits to store address
-define(MAX_FRAME_LENGTH, 60 * 8). % number of bits to store address
-define(MAX_DATA_LENGTH, (?MAX_FRAME_LENGTH - (?ADDRESS_LENGTH * 3 + ?SESSION_MANAGEMENT_LENGTH + ?MESSAGE_TYPE_LENGTH)) / 8). % number of BYTES for data
-define(NET_TRAVERSAL_TIME, 1000).


-define(NETWORK_PROP_LIST, [{address_length, ?ADDRESS_LENGTH},
                                 {net_traversal_time, ?NET_TRAVERSAL_TIME},
                                 {reporting_unit, ?REPORT_UNIT}
                                ]).
-define(DATA_LINK_PROPS_LIST, [{default_state, plc_only}]).
-define(TRANSPORT_PROPS_LIST, [{default_state, disable}]).

-define(PROTOCOL_PROPS_LIST, [{?NETWORK_PROPERTIES, ?NETWORK_PROP_LIST},
                              {?DATA_LINK_PROPERTIES, ?DATA_LINK_PROPS_LIST},
                              {?TRANSPORT_PROPERTIES, ?TRANSPORT_PROPS_LIST}
                             ]).

%%% REPORTING UNIT PROPERTIES
-define(REPORT_UNIT_PROPS_LIST, [{data_server_name,stub_data_server},
                                 {data_server_ip, "192.168.14.6"}
%                                 {data_server_ip, "127.0.0.1"}
                                ]).


%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(MESSAGE_SEND_INTERVAL, 50). % in second

-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
%                         {role, smart_meter}
                         {role, data_concentration_server}
                        ]).
