#!/bin/sh
# Disk color based on worst mount
disk_color=$(df --output=pcent / /home/chris/media /mnt/steamone /mnt/steamtwo \
  | tail -n+2 | tr -d ' %' \
  | awk 'BEGIN{c="#a9b1d6"} {if($1>90)c="#f7768e"; else if($1>75 && c!="#f7768e")c="#e0af68"} END{print c}')

# CPU (two samples 0.2s apart)
cpu=$(awk '{u=$2+$4; t=u+$5; print u, t}' /proc/stat | head -1)
sleep 0.2
cpu2=$(awk '{u=$2+$4; t=u+$5; print u, t}' /proc/stat | head -1)
cpu_pct=$(echo "$cpu $cpu2" | awk '{printf "%.0f", ($3-$1)/($4-$2)*100}')

# Memory
mem=$(free | awk '/^Mem:/{printf "%.0f", $3/$2*100}')

echo "%{F${disk_color}}󰋊%{F-}  %{F#7aa2f7}󰘚 ${cpu_pct}%%{F-}  %{F#7dcfff}󰍛 ${mem}%%{F-}"
