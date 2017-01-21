%Medium Types
-define(RF, 1).
-define(PLC, 2).
-define(RF_PLC, 3).



%Properties atoms
-define(NODE_PROPERTIES, node_properties).
-define(LOAD_NG_CORE_PROPERTIES, load_ng_core_properties).
-define(DATA_LINK_PROPERTIES, data_link_properties).
-define(PROTOCOL_PROPERTIES, protocol_properties).
-define(REPORT_UNIT_PROPERTIES, report_unit_properties).
-define(APPLICATION_PROPERTIES, application_properties).
-define(SELF_ADDRESS, self_address).

%LOADng Messages types
-define(RREQ, 0).
-define(RREP, 1).
-define(RERR, 2).
-define(DATA, 3).


%QLC Atoms
-define(EMPTY_QUERY_RESULT, empty_result).


%Report unit
%send report
-define(MANAGEMENT_MESSAGE, management_message).
-define(DATA_MESSAGE, data_message).
-define(ROUTING_SET, routing_sets).
%receive
-define(UPDATE_CONFIGURATION, update_configuration).
-define(INITIATE_TRANSACTION, initiate_transaction).

%data messages types
-define(DATA_MESSAGE_TYPE, data_message_type).
-define(SEND_MESSAGE, send_message).
-define(RECEIVED_MESSAGE, received_message).
-define(MIDDLE_MESSAGE, middle_message).
