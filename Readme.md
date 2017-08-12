![](http://in.bgu.ac.il/Style%20Library/he-IL/Images/logotextfull.gif)![Alt text](http://in.bgu.ac.il/Style%20Library/Images/bgu/general/logo-simbol.gif)
####Routing and Distributed Monitoring in Smart PLC Networks (p-2017-063)               
#####Reports :    [PDR](https://drive.google.com/open?id=0BwYXH0n9ZthMblJyOXNEZzBxWjg)      [PRE](https://drive.google.com/open?id=0BwYXH0n9ZthMT1l1dHlhZEI4Szg)       [PROGRESS1](https://drive.google.com/open?id=0BwYXH0n9ZthMdjl2YnhHRHBxdEE)       [PROGRESS2](https://drive.google.com/open?id=0BwYXH0n9ZthMOS13ck54SldrLUE)       [FINAL]()       [POSTER](https://drive.google.com/open?id=0B-tdbX-lO15iUGMxUEpsMDJFeHc)
---
#### Environment setup proggress:
##### Ensure that all properties configured to desired parameters in properties.hrl file:
```erlang data_server_ip ```

```erlang grafana_server_ip``` - set atom```erlang undefined``` if not relevant

```erlang APPLICATION_TYPE``` - set atom```erlang other``` for you application

```erlang meters_list```

```erlang MODULES_TO_FILTER``` - to filter modules from log

```erlang LOGGER_MODE``` - set atom```erlang file``` to allow background running of application - path to file no edisson```bash~/node/file.log```


##### Define environment setup in  lab-env.txt file :
For example 
```
132.73.199.156 node_1 dc_app dual
132.73.199.232 node_10 sem dual
132.73.199.238 node_9 sem dual
132.73.199.200 node_4 sem dual
132.73.199.225 node_6 sem dual

132.73.199.234 node_7 simple_dc dual
132.73.192.31 node_14 am_sem dual
132.73.192.42 node_15 am_sem plc_only
132.73.192.80 node_16 am_sem dual
132.73.198.241 local stats_server not_relevant
```

##### Run run-all-nodes.sh to generate starting line 
```scripts/run-all-nodes.sh scripts/dima-env-file.txt Unnamed``` where Unnamed is terminal profile, on lab machine it is Default instead

##### Run Generated line



```bash
scripts/run-all-nodes.sh scripts/lab-env.txt Default % To generate command to run all environment configured in scripts/lab-env.txt

%EXAMPLE:
gnome-terminal --maximize --tab-with-profile=Default -e "scripts/start_node.sh 132.73.199.156 node_1 dc dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.199.232 node_10 am_sem dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.199.234 node_7 am_sem dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.199.238 node_9 am_sem dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.199.225 node_6 am_sem dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.199.200 node_4 am_sem dual" --tab-with-profile=Default \
-e "scripts/start_node.sh 132.73.192.17 node_14 am_sem dual"



% single node start script
cd final-project/node
scripts/start_node.sh 132.73.199.156 node_1 dc plc_only
scripts/start_node.sh 132.73.199.232 node_10 am_sem dual
scripts/start_node.sh 132.73.199.200 node_4 am_sem rf_only

local:
erl -name node_1@132.73.198.5 -setcookie load_ng_project -run c c node -run node start node_1 smart_meter dual

stub_data_server:
erl -name stub_data_server@132.73.198.5 -setcookie load_ng_project -run c c stub_data_server -run c c log -run stub_data_server start

erl -setcookie load_ng_project -name stats_server@127.0.0.1 -run c c loadNGgui -run loadNGgui start


```

###Report Messages Format (from node)
```erlang
{node_state,[{routing_set,[{{destination,0},{next_address,0},{medium,3}}]},{medium_mode,dual}]}

{{management_message,send_message},[{source,1},{destination,1},{id,45085103},{type,0}]}

```
###Routing Set Entry Record
```erlang
-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time, valid}).
```

###Configuration Messages Format (to node)
```erlang
{update_configuration,  [{plc, on/off},{rf, on/off},{reset_node, reset}]

{reset_routing_set,  [}

{initiate_transaction, [{destination, Destination}, {data, Data}]]
```



---
#### Data Server setup progress:
>TODO
>
>
---
#####MAC - STATIC IP in University WiFi Network: fist 10 addreses defined static IP's
|Node  |MAC Address         |     IP Address      |        Validated|
|------|--------------------|---------------------|---------------------|
|server|6c:f0:49:ee:d7:9f   |   NOT STATIC||
|1     |78:4b:87:a3:07:48   |	132.73.199.156|Valid|
|2     |78:4b:87:a6:b7:31   |	132.73.199.164||
|3     |78:4b:87:a5:05:1b   |	132.73.199.188||
|4     |78:4b:87:a8:4b:d5   |	132.73.199.200|Valid|
|5     |90:b6:86:03:62:f5   |	132.73.199.228|Valid|
|6     |90:b6:86:09:6c:64   |	132.73.199.225|Valid|
|7     |90:b6:86:0e:f6:11   |	132.73.199.234|Valid|
|8     |90:b6:86:0e:8c:dd   |	132.73.199.235|not working - Read Only File System - need re-flash|
|9     |90:b6:86:04:31:0a   |	132.73.199.238|Valid|
|10    |90:b6:86:0a:4e:1d   |	132.73.199.232|Valid|		
|11    |78:4b:87:ac:dc:7b   |	132.73.192.104||
|12    |78:4b:87:ad:52:2a   |	132.73.192.176||
|13    |90:b6:86:0e:35:8f   |	132.73.192.195||
|14    |90:b6:86:0e:09:9a   |	132.73.192.17|Valid(132.73.192.31)|
|15    |90:b6:86:08:4d:0c   |	132.73.192.109|132.73.192.42|
|16    |90:b6:86:13:39:21   |	132.73.192.175|132.73.192.80|
|17    |90:b6:86:0d:e4:ec   |	132.73.192.38||
|18    |                    |                  ||
|19    |                    |                  ||
|21    |90:b6:86:03:a1:4d   |	132.73.192.163||
|22    |90:b6:86:04:23:66   |	132.73.192.120|v|
|23    |90:b6:86:0a:72:cb   |	132.73.192.80||
|24    |90:b6:86:09:79:53   |	132.73.192.34|v|
|25    |90:b6:86:04:8c:7c   |	132.73.192.142|v|
|26    |90:b6:86:0b:b8:27   |	132.73.192.180||
|27    |90:b6:86:01:db:77   |	132.73.192.56||
|28    |90:b6:86:09:cc:3c   |	132.73.192.47||



###Protocol Interface (to application)


```erlang
start(ProtocolModule, Properties) ->
    ?LOGGER:debug("[~p]: Starting protocol: ~p, with props: ~w~n", [?MODULE, ProtocolModule, Properties]),
    Timeout = proplists:get_value(timeout, Properties),
    {ok,PID} = gen_server:start_link({local, ?MODULE }, ProtocolModule, Properties, [{timeout, Timeout}]),
    PID.

stop() ->
    gen_server:call(?MODULE, stop).

send(Destination, Data)->
    gen_server:call(?MODULE, {data_message, {Destination, Data}}, ?TIMEOUT).

send_data_request(Destination)->
    gen_server:call(?MODULE, {data_request_message, {Destination}}, ?TIMEOUT).

send_data_reply(Destination, Data)->
    gen_server:call(?MODULE, {data_reply_message, {Destination, Data}}, ?TIMEOUT).

%essential for protocol to be aware of uppel apllication layer
hand_shake(ApplicationPid) ->
    gen_server:call(?MODULE, {hand_shake, ApplicationPid}).

update_configuration(OptionsList)->
    gen_server:call(?MODULE, {update_configuration, OptionsList}).
```
