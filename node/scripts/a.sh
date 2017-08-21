IPS=$(arp -a| grep ether| awk '{print $2}' | sed 's/)//g' | sed 's/(//g')
echo $IPS
