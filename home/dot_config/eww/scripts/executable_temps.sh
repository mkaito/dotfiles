#!/bin/sh
cpu_temp=$(cat /sys/class/hwmon/hwmon2/temp1_input)
gpu_temp=$(cat /sys/class/hwmon/hwmon5/temp1_input)
gpu_load=$(cat /sys/class/drm/card0/device/gpu_busy_percent)
echo "{\"cpu_temp\":$((cpu_temp/1000)),\"gpu_temp\":$((gpu_temp/1000)),\"gpu_load\":${gpu_load}}"
