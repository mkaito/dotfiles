#!/bin/bash
 
sudo pacman -Sy
 
echo "" > $HOME/.pacmanupdates
 
for i in `pacman -Qu`;
do echo `pacman -Ss $i` >> $HOME/.pacmanupdates
done
