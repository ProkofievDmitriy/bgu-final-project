
%%% SYSTEM PROPERTIES
-define(CURRENT_LOG_LEVEL, 0). % 0- DEBUG, 1 - INFO, 2 - WARN, 3 - ERROR


%%% NODE PROPERTIES
%-define(NETWORK_DEVICE, "enp2s0").
-define(NETWORK_DEVICE, "wlp3s0").

-define(LOGGER, log).
-define(LOAD_NG, load_ng).
-define(LOAD_NG_CORE, load_ng_core).
-define(DATA_LINK, data_link).
-define(APPLICATION, simple_app).


-define(NODE_RESOURCES, [?LOGGER, ?LOAD_NG, ?LOAD_NG_CORE, ?DATA_LINK, ?APPLICATION]).


%%% hy-LOADng PROPERTIES


%%% REPORTING UNIT PROPERTIES



