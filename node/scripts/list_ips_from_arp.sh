#!/bin/bash

# echo "INFO : On node_1 ($1) extracting ips from arp table"
  sshpass -p zSs3cfmx scp scripts/a.sh root@$1:/home/root/
  sshpass -p zSs3cfmx ssh -T -o StrictHostKeyChecking=no root@$1 << EOF
  chmod +x a.sh
EOF
 	RESULT=$(sshpass -p zSs3cfmx ssh -T -o StrictHostKeyChecking=no root@$1 ./a.sh)

echo $RESULT
