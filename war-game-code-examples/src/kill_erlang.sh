#bin/bash

TOKILL=$(pgrep beam)
if [ -z "$TOKILL" ];
	then
  		echo "IFNO : Nothing to kill ..."
    else 
    	echo "INFO : Erlang proccess pid to kill $TOKILL"
		kill $TOKILL
fi
