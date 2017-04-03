![](http://in.bgu.ac.il/Style%20Library/he-IL/Images/logotextfull.gif)![Alt text](http://in.bgu.ac.il/Style%20Library/Images/bgu/general/logo-simbol.gif)
####Routing and Distributed Monitoring in Smart PLC Networks (p-2017-063)               
#####Reports :    [PDR](https://drive.google.com/open?id=0BwYXH0n9ZthMblJyOXNEZzBxWjg)      [PRE](https://drive.google.com/open?id=0BwYXH0n9ZthMT1l1dHlhZEI4Szg)       [PROGRESS1](https://drive.google.com/open?id=0BwYXH0n9ZthMdjl2YnhHRHBxdEE)       [PROGRESS2]()       [FINAL]()       [POSTER]()
---
#### Node setup proggress:
>TODO
>
>
```bash
cd final-project/node
scripts/start_node.sh 132.73.199.156 node_1 data_concentration_server plc_only
scripts/start_node.sh 132.73.199.232 node_10 data_concentration_server dual
scripts/start_node.sh 132.73.199.200 node_4 smart_meter rf_only

local:
erl -name node_1@132.73.198.5 -setcookie load_ng_project -run c c node -run node start node_1 smart_meter dual

stub_data_server:
erl -name stub_data_server@132.73.198.5 -setcookie load_ng_project -run c c stub_data_server -run c c log -run stub_data_server start


```

###Report Messages Format (from node)
```erlang
{data_message, [{data_message_type, send_message/received_message/middle_message},
		        {report_time, 312731767}, %current time in millis
	            {source, Originator},
		        {destination,  Destination},
		        {message_id, hash(Originator ++ Destination ++ Data)},
                {data, MessageData}			 
		        ]}

{management_message, [{message_type, ?RREQ/?RREP/?RERR/?RACK}, %Should not be sent, not relevant to data server : Managment is Managment
			          {report_time, 312731767}, %current time in millis
			          {source, Source},
			          {destination, Destination}				 
	            	 ]}

{routing_set, [{node_name, node_1/2/3/4/5/6.../31},
               {report_time, 312731767},
               {data, RoutingSetRecordsAsList}
              ]}
```
###Routing Set Entry Record
```erlang
-record(routing_set_entry, {dest_addr, next_addr, medium, hop_count, r_seq_number, bidirectional, valid_time, valid}).
```

###Deddy we shouldn't use this record in your server, he should be independent from protocol implementation.
####You need to define which data you need to describe routing set entry:
```erlang
[{next_hop, NextHopAddress}, {destination, DestinationAddress}, {medium, Medium}]
```


###Configuration Messages Format (to node)
```erlang
{update_configuration,  [{plc, on/off},{rf, on/off},{reset_node, reset}]

{node_state,  [{medium_mode, plc/rf/dual/off} , {node_name, node_1} , {routing_set, [RoutingSetRecordsEntry,...]}

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
|7     |90:b6:86:0e:f6:11   |	132.73.199.234||
|8     |90:b6:86:0e:8c:dd   |	132.73.199.235||
|9     |90:b6:86:04:31:0a   |	132.73.199.238|Valid|
|10    |90:b6:86:0a:4e:1d   |	132.73.199.232|Valid|		
|11    |78:4b:87:ac:dc:7b   |	132.73.192.104||
|12    |78:4b:87:ad:52:2a   |	132.73.192.176||
|13    |90:b6:86:0e:35:8f   |	132.73.192.195||
|14    |90:b6:86:0e:09:9a   |	132.73.192.164||
|15    |90:b6:86:08:4d:0c   |	132.73.192.109||
|16    |90:b6:86:13:39:21   |	132.73.192.175||
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
