#!/bin/bash


echo "IMPORTANT NOTICE : Make sure you instaled sshpass (sudo apt-get install sshpass) before you starting !!!"
if [ -z "$1" ]
  then
  	echo "ERROR : "
    echo "Usage : ./run-all-nodes.sh <env-file-name> <current-bash-profile-name>: e.g - ./run-all-nodes.sh env-file.txt Default"
    exit
fi

if [ -z "$2" ]
  then
  	echo "ERROR : "
    echo "Usage : ./run-all-nodes.sh <env-file-name> <current-bash-profile-name>: e.g - ./run-all-nodes.sh env-file.txt Default"
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


function start {
  echo "INFO : Initializing node with params - $1 $2 $3 $4"
  gnome-terminal --window-with-profile=$5 -e "scripts/start_node.sh $1 $2 $3 $4"

}


CMD=""
echo "Loading Setup from : $1"
while IFS='' read -r line; do
    echo "node setup: $line"
    RES=$(concat $line $2)
    CMD="$CMD $RES"
done < $1
echo "gnome-terminal --maximize$CMD"
CMD="gnome-terminal --maximize$CMD"
$CMD
