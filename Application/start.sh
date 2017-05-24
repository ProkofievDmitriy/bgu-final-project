#!/bin/sh

echo "starting_script"


gnome-terminal -e "erl -sname dc_node" "c(simple_prot)."

gnome-terminal -e "erl -sname sem1_node"


