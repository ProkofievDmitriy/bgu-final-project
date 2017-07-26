-define(MODULES_TO_FILTER, [utils, report, data_link, modem_port, transport, load_ng, node, application_interface, stats_server_interface]).
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
                         dc, am_sem,sm_sem, sem_naive, dc_naive, simple_app, simple_dc,
                         stats_server_interface]).






%%% hy-LOADng PROPERTIES
-define(BROADCAST_ADDRESS, 0).
-define(LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS, 720000).
-define(NODE_STATUS_TIMER_INTERVAL, 3000).
-define(REMOVE_NOT_VALID_ROUTES_TIMER, ?LOAD_NG_ROUTE_VALID_TIME_IN_MILLIS + 500).

%TODO Address length and message type currently should give 2 bytes for correct working (crc32 in modem port calculation) - integer number of bytes (not bitstring), should be fixed
-define(MEDIUM_LENGTH, 2). % number of bits to store address
-define(ADDRESS_LENGTH, 6). % number of bits to store address
-define(MESSAGE_TYPE_LENGTH, 4). % number of bits to store address
-define(MESSAGE_UUID_LENGHT, 8). % number of bits to store address
-define(SESSION_MANAGEMENT_LENGTH, 4). % number of bits to store address
-define(SESSION_ID_LENGHT, 8). % number of bits to store address
-define(TRANSPORT_HEADER_LENGTH, ?SESSION_MANAGEMENT_LENGTH * 2 + ?SESSION_ID_LENGHT). % number of bits to store address
-define(DATA_LENGTH_SIZE, 16). % number of bits to store address
-define(MAX_FRAME_LENGTH, 54 * 8). % number of bits to store address
-define(MAX_DATA_LENGTH, (?MAX_FRAME_LENGTH - (?ADDRESS_LENGTH * 4 + ?SESSION_MANAGEMENT_LENGTH + ?MESSAGE_UUID_LENGHT + ?MESSAGE_TYPE_LENGTH + ?DATA_LENGTH_SIZE)) / 8). % number of BYTES for data
-define(MAX_DATA_LENGTH_IN_BITS, (48 + ?MAX_FRAME_LENGTH - (?MEDIUM_LENGTH + ?ADDRESS_LENGTH * 4 + ?TRANSPORT_HEADER_LENGTH + ?MESSAGE_UUID_LENGHT + ?MESSAGE_TYPE_LENGTH + ?DATA_LENGTH_SIZE))). % number of BYTES for data
-define(NET_TRAVERSAL_TIME, 4000).
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
                               {timeout, ?TIMEOUT}
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
                                %  {data_server_ip, "132.73.198.241"}
                                 {data_server_ip, "192.168.14.30"} %Dima home server
                                %  {data_server_ip, "127.0.0.1"}
                                ]).



%%% SIMPLE APPLICATION PROPERTIES
-define(APPLICATION_NAME, simple_application).
% -define(APPLICATION_NAME, other).
-define(MESSAGE_SEND_INTERVAL, 5). % in second

-define(APP_PROPS_LIST, [{app_name, ?APPLICATION_NAME},
                         {send_message_interval, ?MESSAGE_SEND_INTERVAL},
%                         {role, smart_meter}
                         {role, data_concentration_server},
                        %  {meters_list, [node_7]}
                         {meters_list, [node_10, node_4, node_6, node_7, node_9, node_14, node_15, node_16]}
                        %  {meters_list, [node_10, node_4, node_7, node_14]}
                        %  {meters_list, [node_22, node_25]}
                        ]).
