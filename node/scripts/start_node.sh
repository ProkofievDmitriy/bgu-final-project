#!/bin/bash


if [ -z "$1" ]
  then
  	echo "ERROR : no ip provided"
    echo "Usage : ./start_node.sh <ip> <node_name> : e.g - ./start_node.sh 192.168.14.53 node_10"
    exit 1
fi
if [ -z "$2" ]
  then
  	echo "ERROR : no node name provided"
    echo "Usage : ./start_node.sh <ip> <node_name> : e.g - ./start_node.sh 192.168.14.53 node_10"
    exit 1
fi


PASSWORD="zSs3cfmx"

 	echo "INFO : On node $2@$1 passwrod: $PASSWORD: Removing old files if exists..."
 	sshpass -p $PASSWORD ssh -T -o StrictHostKeyChecking=no root@$1 << EOF
 	rm -rf ./node
EOF

 	echo "INFO : On node $2@$1 : copying files to node ... "
 	PWD=$(pwd)
  find -name "*.dump" | xargs rm -f $1
  find -name "*.beam" | xargs rm -f $1
  rm -f ./port
 	sshpass -p $PASSWORD scp -r $PWD/ root@$1:/home/root/

sshpass -p $PASSWORD ssh -o StrictHostKeyChecking=no root@$1 << EOF
pushd ./node/scripts
chmod +x ./kill_erlang.sh
chmod +x ./uart_config.sh
chmod +x ./fg_start_erlang.sh
./kill_erlang.sh
popd
EOF

	echo "INFO : On node $2@$1 : Starting erlang in fg mode ..."
    sshpass -p $PASSWORD ssh -T -o StrictHostKeyChecking=no root@$1  "./node/scripts/fg_start_erlang.sh $2 wlan0"
