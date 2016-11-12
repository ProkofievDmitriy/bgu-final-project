#!/bin/bash



node_names="tl tr bl br"
nodes_array=( $node_names )
local_ips="127.0.0.1 127.0.0.1 127.0.0.1 127.0.0.1"
final_nodes=( )
final_nodes+="["

ips=( $local_ips)

\cp /home/dmitriy/eclipse_workspaces/erlang/proj/src/*.erl /home/dmitriy/war_game/src
\cp /home/dmitriy/eclipse_workspaces/erlang/proj/include/*.hrl /home/dmitriy/war_game/include


for index in "${!ips[@]}"
do
    echo "INFO : Initializing node - '${nodes_array[index]}@${ips[index]}'"
    gnome-terminal --tab -e "erl -name ${nodes_array[index]}@${ips[index]} -setcookie asd -run c cd ./src"
    final_nodes+="'${nodes_array[index]}@${ips[index]}'"
	if [ $index -ne "3" ]
    	then
    	final_nodes+=","
    fi
done

final_nodes+="]"
echo "$final_nodes"

MYIP=$(ip addr show $net_interface | grep "inet\b" | awk '{print $2}' | cut -d/ -f1)

echo "INFO : Starting local erlang node in 60 seconds , awaiting for proper remote nodes staring..."
sleep 3
exec erl -name g@127.0.0.1 -setcookie asd -run c cd ./src -run c c g -run g test_network $final_nodes -run g start
