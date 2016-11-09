a=4
echo "a is $a"
STR_HOST=$(hostname -v)
STR_IP=$(ifconfig wlan0 | grep "inet " | awk -F'[: ]+' '{ print $4 }')
STR_AT=@
#STR_IP=$(ip addr show wlan0 | grep -Po 'inet \K[\d.]+')

echo "STR_HOST IS: $STR_HOST"
echo "STR_IP IS: $STR_IP"

NAME=$STR_HOST$STR_AT$STR_IP
echo $NAME

ERL_CMD_A="nohup erl -noshell -s node start -s init stop -name "
ERL_CMD_B=" -setcookie isg"
ERL_CMD=$ERL_CMD_A$NAME$ERL_CMD_B
echo "ERL_CMD is: $ERL_CMD"



cd /ISG/node
gcc port.c -o port
erlc  node.erl server_port.erl modem_port.erl hyRPL.erl isg_server.erl db.erl station.erl root.erl app.erl
$ERL_CMD
#erl -noshell -s node start -s init stop -name node_7@132.73.199.234 -setcookie isg 
