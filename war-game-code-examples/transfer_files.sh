#!/bin/bash

 	echo "INFO : On node $2@$1 : Removing old files if exists..."
 	sshpass -p temppwd ssh -T -o StrictHostKeyChecking=no ubuntu@$1 << EOF
 	rm -rf war_game
EOF

 	echo "INFO : On node $2@$1 : copying files to node ... "
 	PWD=$(pwd)
 	sshpass -p temppwd scp -r $PWD/ ubuntu@$1:/home/ubuntu/

    sshpass -p temppwd ssh -o StrictHostKeyChecking=no ubuntu@$1 << EOF
    cd war_game/src
	chmod +x ./kill_erlang.sh   
    chmod +x ./fg_start_erlang.sh
    ./kill_erlang.sh
EOF

	echo "INFO : On node $2@$1 : Starting erlang in fg mode ..."
    sshpass -p temppwd ssh -o StrictHostKeyChecking=no ubuntu@$1  "./war_game/src/fg_start_erlang.sh $2 eth0"
