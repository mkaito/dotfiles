#!/bin/sh
df -h --output=target,size,used,avail,pcent / /home/chris/media /mnt/steamone /mnt/steamtwo \
  | tail -n+2 \
  | awk 'BEGIN{printf "["} NR>1{printf ","} {gsub(/%/,"",$5); printf "{\"mount\":\"%s\",\"size\":\"%s\",\"used\":\"%s\",\"avail\":\"%s\",\"pcent\":%s}",$1,$2,$3,$4,$5} END{print "]"}'
