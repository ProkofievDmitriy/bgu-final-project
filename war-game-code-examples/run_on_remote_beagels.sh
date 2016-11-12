#!/bin/bash


echo "IMPORTANT NOTICE : Make sure you instaled sshpass (sudo apt-get install sshpass) before you starting !!!"
if [ -z "$1" ]
  then
  	echo "ERROR : "
    echo "Usage : ./init_game <net_interface_with_beagles> : e.g - ./init_game enp2s0 or ./init_game eth0"
    exit
fi
net_interface=$1
LINES=$(arp -a| grep $net_interface | grep ether| awk '{print $2}' | sed 's/)//g' | sed 's/(//g')

node_names="tl tr bl br"
nodes_array=( $node_names )
final_nodes=( )
final_nodes+="["

ips=( $LINES )

if [ ${#ips[@]} -ne "4" ]
  then
    echo "ERROR : Something wrong with your connection, found less then 4 beagles
    		1. Disconnect switch from main computer, and wait 30 - 60 seconds
    		2. Plug it back and reboot beagles
    		3. Make sure you entering right network interfase
    		4. Rerun script
    		5. Goodluck"
    exit
fi	

for index in "${!ips[@]}"
do
    echo "INFO : Initializing node - '${nodes_array[index]}@${ips[index]}'"
    gnome-terminal --tab -e "./transfer_files.sh ${ips[index]} ${nodes_array[index]}"
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
sleep 50
exec erl -name g@$MYIP -setcookie asd -run c cd ./src -run c c g -run g test_network $final_nodes -run g start
