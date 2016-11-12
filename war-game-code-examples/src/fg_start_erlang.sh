#bin/bash
if [ -z "$1" ]
  then
    echo "Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip>"
    exit
fi

if [ -z "$2" ]
  then
    echo "Usage : ./start_erlang <node_name_atom> <network_device_for_getting_ip>"
    exit
fi



IP=$(ip addr show $2 | grep "inet\b" | awk '{print $2}' | cut -d/ -f1)

echo "Current ip is '$IP'"
echo "Starting erlang with : erl -name '$1@$IP' -setcookie asd -run c cd ./war_game/src"

exec erl -name $1@$IP -setcookie asd -run c cd ./war_game/src