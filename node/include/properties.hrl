-define(MODULES_TO_FILTER, [load_ng, report, node, transport]).
%-define(MODULES_TO_FILTER, [report, load_ng, transport]).




%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, -1). % -1 - DEBUG+, 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR, 5 - TemoraryINFO, 4 - Critical

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






%%% hy-LOADng PROPERTIES
-define(BROADCAST_ADDRESS, 0).
-define(LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS, 20000).
-define(NODE_STATUS_TIMER_INTERVAL, 20000).
-define(REMOVE_NOT_VALID_ROUTES_TIMER, ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS * 2).

%TODO Address length and message type currently should give 2 bytes for correct working (crc32 in modem port calculation) - integer number of bytes (not bitstring), should be fixed
-define(ADDRESS_LENGTH, 6). % number of bits to store address
-define(MESSAGE_TYPE_LENGTH, 8). % number of bits to store address
-define(SESSION_MANAGEMENT_LENGTH, 8). % number of bits to store address
-define(DATA_LENGTH_SIZE, 8). % number of bits to store address
-define(MAX_FRAME_LENGTH, 60 * 8). % number of bits to store address
-define(MAX_DATA_LENGTH, (?MAX_FRAME_LENGTH - (?ADDRESS_LENGTH * 4 + ?SESSION_MANAGEMENT_LENGTH + ?MESSAGE_TYPE_LENGTH + ?DATA_LENGTH_SIZE)) / 8). % number of BYTES for data
-define(NET_TRAVERSAL_TIME, 500).
-define(TIMEOUT, ?NET_TRAVERSAL_TIME * 3).
-define(ACK_REQUIRED, 0). % 0 = false, 1 = true
-define(SEQUENCE_NUMBER_MAX_VALUE, 256). %maximum value for sequence number


-define(NODE_PROPS_LIST, [{protocol, ?LOAD_NG},
                          {timeout, ?TIMEOUT}
%                          {node_name, node_1} % temp value , need to be define per each node.
                         ]).


-define(NETWORK_PROP_LIST, [{address_length, ?ADDRESS_LENGTH},
                                 {net_traversal_time, ?NET_TRAVERSAL_TIME},
                                 {reporting_unit, ?REPORT_UNIT},
                                 {timeout, ?TIMEOUT}
                                ]).
-define(DATA_LINK_PROPS_LIST, [
%                               {default_state, dual},
%                               {default_state, plc_only},
                               {default_state, rf_only},
                               {timeout, ?TIMEOUT}
                               ]).
-define(TRANSPORT_PROPS_LIST, [{default_state, disable},
                               {timeout, ?TIMEOUT}
                               ]).

-define(PROTOCOL_PROPS_LIST, [{?NETWORK_PROPERTIES, ?NETWORK_PROP_LIST},
                              {?DATA_LINK_PROPERTIES, ?DATA_LINK_PROPS_LIST},
                              {?TRANSPORT_PROPERTIES, ?TRANSPORT_PROPS_LIST},
                              {timeout, ?TIMEOUT}
                             ]).

%%% REPORTING UNIT PROPERTIES
-define(REPORT_UNIT_PROPS_LIST, [{data_server_name,stub_data_server},
                                 {data_server_ip, "132.73.198.5"}
%                                 {data_server_ip, "127.0.0.1"}
                                ]).


%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
-define(MESSAGE_SEND_INTERVAL, 5). % in second

-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
%                         {role, smart_meter}
                         {role, data_concentration_server}
                        ]).
