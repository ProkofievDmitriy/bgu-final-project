#!/bin/bash

if [ -z "$1" ]
  then
    echo "Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip> <Application Role Type>"
    exit
fi

if [ -z "$2" ]
  then
    echo "Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip> <Application Role Type>"
    exit
fi


if [ -z "$3" ]
  then
    echo "Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip> <Application Role Type>"
    exit
fi


IP=$(ip addr show $2 | grep "inet\b" | cut -d " " -f6 | cut -d/ -f1)

echo "Current ip is '$IP'"
ERLANG_COMMAND="/usr/local/lib/erlang/bin/erl -name $1@$IP -setcookie load_ng_project -run c cd ./node -run c c node -run node start $1  $3"
echo "Starting erlang Remotely with : $ERLANG_COMMAND"
exec $ERLANG_COMMAND