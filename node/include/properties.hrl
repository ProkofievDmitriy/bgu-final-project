-define(MODULES_TO_FILTER, [utils, report, transport, data_link, modem_port, application_interface, stats_server_interface]).
% -define(MODULES_TO_FILTER, [load_ng, transport, stats_server_interface]).
% -define(MODULES_TO_FILTER, [stats_server_interface, report]).




%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % -1 - DEBUG+, 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR, 4 - Critical, 5 - TemoraryINFO

-define(LOGGER, log).
-define(LOGGER_MODE, dual).

-define(LOAD_NG, load_ng).
-define(NETWORK, load_ng_core).
-define(DATA_LINK, data_link).
-define(REPORT_UNIT, report).
-define(MODEM_PORT, modem_port).
-define(PROTOCOL, protocol_interface).
-define(TRANSPORT, transport).

%%% NODE PROPERTIES
%-define(NETWORK_DEVICE, "enp2s0").

-define(NETWORK_DEVICE, "wlp3s0").
% -define(NETWORK_DEVICE, "wlan0").

-define(NODE_RESOURCES, [layer_interface,
                         ?LOGGER,
                         ?PROTOCOL,
                         ?REPORT_UNIT,
                         ?NETWORK,
                         ?DATA_LINK,
                         ?TRANSPORT,
                         ?LOAD_NG,
                         ?MODEM_PORT,
                         utils, ntp, sntp,
                         application_interface,
                         node_control_interface,
                         isg_time,
                         dc_naive, simple_app, simple_dc, dc_app, sem, app_utils,
                         stats_server_interface]).






%%% hy-LOADng PROPERTIES
-define(BROADCAST_ADDRESS, 0).
-define(LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS, 720000).
-define(NODE_STATUS_TIMER_INTERVAL, 3000).
-define(REMOVE_NOT_VALID_ROUTES_TIMER, ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS + 500).

-define(MEDIUM_LENGTH, 2). % number of bits to store address
-define(ADDRESS_LENGTH, 6). % number of bits to store address
-define(MESSAGE_TYPE_LENGTH, 4). % number of bits to store address
-define(MESSAGE_UUID_LENGHT, 8). % number of bits to store address
-define(SESSION_MANAGEMENT_LENGTH, 4). % number of bits to store address
-define(SESSION_ID_LENGHT, 8). % number of bits to store address
-define(DATA_LENGTH_SIZE, 16). % number of bits to store address
-define(MAX_FRAME_LENGTH, 54 * 8). % 54 * 8

-define(TRANSPORT_HEADER_LENGTH, ?SESSION_MANAGEMENT_LENGTH * 2 + ?SESSION_ID_LENGHT).
-define(DATA_LINK_HEADER_LENGTH, ?ADDRESS_LENGTH * 2).
-define(NETWORK_HEADER_LENGTH, ?MEDIUM_LENGTH + ?ADDRESS_LENGTH * 3 + ?MESSAGE_UUID_LENGHT + ?MESSAGE_TYPE_LENGTH + ?DATA_LENGTH_SIZE).

-define(MAX_DATA_LENGTH_IN_BITS, (?MAX_FRAME_LENGTH - (?NETWORK_HEADER_LENGTH + ?TRANSPORT_HEADER_LENGTH + ?DATA_LINK_HEADER_LENGTH))). % number of bits for data
-define(MAX_DATA_LENGTH, (?MAX_DATA_LENGTH_IN_BITS / 8)). % number of BYTES for data


-define(NET_TRAVERSAL_TIME, 3000).
-define(TIMEOUT, ?NET_TRAVERSAL_TIME * 3).
-define(ACK_REQUIRED, 0). % 0 = false, 1 = true
-define(PATH_ACCUMULATION_ENABLED, 1). % 0 = false, 1 = true
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
                               {timeout, ?TIMEOUT},
                               {nodes_to_filter, []}
                               ]).
-define(TRANSPORT_PROPS_LIST, [{default_state, idle},
                               {timeout, ?TIMEOUT}
                               ]).

-define(PROTOCOL_PROPS_LIST, [{?NETWORK_PROPERTIES, ?NETWORK_PROP_LIST},
                              {?DATA_LINK_PROPERTIES, ?DATA_LINK_PROPS_LIST},
                              {?TRANSPORT_PROPERTIES, ?TRANSPORT_PROPS_LIST},
                              {timeout, ?TIMEOUT}
                             ]).

%%% REPORTING UNIT PROPERTIES
-define(REPORT_UNIT_PROPS_LIST, [{data_server_interface, stats_server_interface},
                                 {data_server_name, stats_server},
                                 {data_server_ip, "132.73.204.17"}
                                %  {data_server_ip, "192.168.14.30"} %Dima home server
                                %  {data_server_ip, "127.0.0.1"}
                                ]).



%%% SIMPLE APPLICATION PROPERTIES
% -define(APPLICATION_TYPE, simple_application).
-define(APPLICATION_TYPE, other).

-define(MESSAGE_SEND_INTERVAL, 5). % in second

-define(APP_PROPS_LIST, [{send_message_interval, ?MESSAGE_SEND_INTERVAL},
%                         {role, smart_meter}
                         {role, data_concentration_server},
                        %  {meters_list, [node_7]}
                         {meters_list, [node_10, node_4, node_6, node_9]}
                        %  {meters_list, [node_10, node_4, node_7, node_14]}
                        %  {meters_list, [node_22, node_25]}
                        ]).
