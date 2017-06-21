#!/bin/bash

TOKILL=$(ps | less | grep erl | cut -d " " -f$1 | head -n $2)
if [ -z "$TOKILL" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else

    	echo "INFO : Erlang proccess pid to kill $TOKILL"
		kill -9 $TOKILL
fi



TOKILL2=$(ps | less | grep beam | cut -d " " -f$1 | head -n $2)
if [ -z "$TOKILL2" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else

    	echo "INFO : Erlang proccess pid to kill $TOKILL2"
		kill -9 $TOKILL2
fi



TOKILL3=$(ps | less | grep erl | cut -d " " -f$1 | head -n $2)
if [ -z "$TOKILL3" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else

    	echo "INFO : Erlang proccess pid to kill $TOKILL3"
		kill -9 $TOKILL3
fi




TOKILL3=$(ps | less | grep beam.smp | cut -d " " -f$1 | head -n $2)
if [ -z "$TOKILL4" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else

    	echo "INFO : Erlang proccess pid to kill $TOKILL4"
		kill -9 $TOKILL4
fi
