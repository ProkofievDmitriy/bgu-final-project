#!/bin/bash
if [ -z "$1" ]
  then
    echo "Usage : ./start_single_local_node <node_name>"
    exit
fi


node_names="tl tr bl br"
nodes_array=( $node_names )
flag=0

echo "INFO : Checking entered node name correctnes...'"

for index in "${!nodes_array[@]}"
do
	if [ ${nodes_array[index]} == $1 ]
    	then
    	flag+=1
    fi
done

if [ $flag -eq "0" ]
  then
    echo "ERROR : You entered WRONG node name, choose one from : tr ,tl ,br, bl ..."
    exit
fi

exec erl -name $1@127.0.0.1 -setcookie asd -run c cd ./src 
