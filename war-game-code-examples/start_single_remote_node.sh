#!/bin/bash
if [ -z "$1" ]
  then
    echo "Usage : ./start_erlang <ip> <node_name>"
    exit
fi

if [ -z "$2" ]
  then
    echo "Usage : ./start_erlang <ip> <node_name>"
    exit
fi

sshpass -p temppwd ssh -o StrictHostKeyChecking=no ubuntu@$1 << EOF
    cd war_game/src
	chmod +x ./kill_erlang.sh   
    ./kill_erlang.sh
EOF


ip=$1
node_name=$2
echo "INFO : On node $2@$1 : Starting erlang in fg mode ..."
sshpass -p temppwd ssh -o StrictHostKeyChecking=no ubuntu@$ip  "./war_game/src/fg_start_erlang.sh $node_name eth0"
