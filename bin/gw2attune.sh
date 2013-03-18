#!/bin/bash
 
sleep 15
 
OLDPID='a'
 
while true; do  #check if gw2 is running
 
				GW2PID=`ps -A | grep -m 1 'Gw2' | awk '{print $1}'`
				WINESVRPID=`ps -A | grep -m 1 'wineserver' | awk '{print $1}'`
				WINEDEVPID=`ps -A | grep -m 1 'winedevice.exe' | awk '{print $1}'`
 
				if [ ! $GW2PID ]; then
								echo 'Guild Wars 2 is not running'
								echo 'Processor is downgrading'
				elif [ $OLDPID != $GW2PID ]; then
												echo 'Guild Wars 2 is getting tuned'
												OLDPID=$GW2PID
												taskset -pc 0-3 $WINESVRPID     #set to use core 0 to 3 ( quadcore )
												taskset -pc 0-3 $WINEDEVPID
												taskset -pc 0-3 $GW2PID
												echo 'Processor is optimizing'
												#cpufreq &
												exit    #ends the script
								else
												echo 'Guild Wars 2 is already tuned.'
				fi
				sleep 15
done
