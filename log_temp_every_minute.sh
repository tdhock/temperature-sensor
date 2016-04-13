#!/bin/bash
while true; do
    echo $(date +%Y-%m-%d_%H:%M) $(bash get_current_temp.sh) >> time_degreesC.log
    sleep 60
done



