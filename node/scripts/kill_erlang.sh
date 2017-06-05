#!/bin/bash

TOKILL=$(ps | less | grep beam | head -n 1 | cut -d " " -f3)
if [ -z "$TOKILL" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else 
    	echo "INFO : Erlang proccess pid to kill $TOKILL"
		kill -9 $TOKILL
fi
