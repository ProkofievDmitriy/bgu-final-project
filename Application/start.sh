#!/bin/sh

echo "starting_script"


gnome-terminal -e "erl -sname dc_node_1 -run c c dc -run c c log" 

gnome-terminal -e "erl -sname sem1_node_1 -run c c am_sem -run c c log"


