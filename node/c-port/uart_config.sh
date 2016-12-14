echo 214 > /sys/class/gpio/export 2>&1
echo high > /sys/class/gpio/gpio214/direction
echo low > /sys/class/gpio/gpio214/direction

##############################
#set pin1 (tx) as uart (mode1)
##############################
echo 131 > /sys/class/gpio/export 2>&1
echo mode1 > /sys/kernel/debug/gpio_debug/gpio131/current_pinmux
#set pin1 as output
echo 249 > /sys/class/gpio/export 2>&1
echo high > /sys/class/gpio/gpio249/direction

#echo 1 > /sys/class/gpio/gpio249/value

#pin1's pullup ressistor
echo 217 > /sys/class/gpio/export 2>&1
echo high > /sys/class/gpio/gpio217/direction

#set pin1's direction as output
echo out > /sys/class/gpio/gpio131/direction

##############################
#set pin0 (rx) as uart (mode1)
##############################
echo 130 > /sys/class/gpio/export 2>&1
echo mode1 > /sys/kernel/debug/gpio_debug/gpio130/current_pinmux
#set pin1 as input
echo 248 > /sys/class/gpio/export 2>&1
echo low > /sys/class/gpio/gpio248/direction

#pin0's pullup ressistor
echo 216 > /sys/class/gpio/export 2>&1
echo in > /sys/class/gpio/gpio216/direction



echo high > /sys/class/gpio/gpio214/direction

