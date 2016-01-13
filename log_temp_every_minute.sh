#!/bin/bash
while true; do
    echo $(date +%Y-%m-%d_%H:%M) $(head -3 /dev/ttyACM0|grep -v ^$|tail -1) >> time_degreesC.log
    sleep 60
done



