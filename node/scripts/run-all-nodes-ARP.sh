#!/bin/bash


echo "IMPORTANT NOTICE : Make sure you instaled sshpass (sudo apt-get install sshpass) before you starting !!!"
if [ -z "$1" ]
  then
  	echo "ERROR : "
    echo "Usage : ./run-all-nodes.sh <NODE_1 IP> <current-bash-profile-name>: e.g - ./run-all-nodes.sh env-file.txt Default"
    exit
fi

if [ -z "$2" ]
  then
  	echo "ERROR : "
    echo "Usage : ./run-all-nodes.sh <NODE_1 IP> <current-bash-profile-name>: e.g - ./run-all-nodes.sh env-file.txt Default"
    exit
fi

function concat {
  if [ "stats_server" == $3 ]
    then
      echo "--tab-with-profile=$5 -e \"erl -setcookie load_ng_project -name stats_server@$1 -run c cd ../Server -run c c node_control_interface -run c c loadNGgui -run c c stats_server -run loadNGgui start\""
    else
      echo "--tab-with-profile=$5 -e \"scripts/start_node.sh $1 $2 $3 $4\""
  fi
}


LINES=$(scripts/list_ips_from_arp.sh $1)
MYIP=$(ip addr show ra0 | grep "inet\b" | awk '{print $2}' | cut -d/ -f1)

CMD=""
NODES=""
flag=0

ips=( $LINES )
nodeNumberOffset=2

for index in "${!ips[@]}"
do
  if [ $MYIP != ${ips[index]} ]
  then
    if ping ${ips[index]} -c 1 > /dev/null
    then
      line="${ips[index]} node_$(expr $index + $nodeNumberOffset) sem dual"
      if [ $index == 0 ]
      then
        NODES="node_$(expr $index + $nodeNumberOffset)"
      else
        NODES="$NODES, node_$(expr $index + $nodeNumberOffset)"
      fi
      echo "node setup: $line"
      RES=$(concat $line $2)
      CMD="$CMD $RES"
    else
      echo "PING RESULT: $?, SKIPPING ${ips[index]}"
    fi
  fi
done
#add stats server
CMD="$CMD $(concat $MYIP local stats_server not_relevant $2)"
#add node_1
CMD="$CMD $(concat $1 node_1 dc_app dual $2)"
NODES="[$NODES]"
echo "starting nodes : $NODES"
newNodesList="{meters_list, $NODES}"
sed -i "/.*{meters_list,.*/c              $newNodesList" include/properties.hrl


echo; echo "Copy And Run GENERATED LINE:"; echo
echo "gnome-terminal --maximize$CMD"; echo; echo
# CMD="gnome-terminal --maximize$CMD"
# $CMD
gnome-terminal --maximize$CMD
