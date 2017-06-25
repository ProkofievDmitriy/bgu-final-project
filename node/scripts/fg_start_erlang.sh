#!/bin/bash
UsageMessage="Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip> <Application Role Type> <data_link_start_state>"
if [ -z "$1" ]
  then
    echo $UsageMessage
    exit
fi

if [ -z "$2" ]
  then
    echo $UsageMessage
    exit
fi


if [ -z "$3" ]
  then
    echo $UsageMessage
    exit
fi


if [ -z "$4" ]
  then
    echo $UsageMessage
    exit
fi


IP=$(ip addr show $2 | grep "inet\b" | cut -d " " -f6 | cut -d/ -f1)

echo "Current ip is '$IP'"
date --set "2017-04-19 00:00:00"
ERLANG_COMMAND="nohup /usr/local/lib/erlang/bin/erl -noinput -noshell -name $1@$IP -setcookie load_ng_project -run c cd ./node -run c c node -run node start $1 $3 $4"
echo "Starting erlang Remotely with : $ERLANG_COMMAND"
$ERLANG_COMMAND
