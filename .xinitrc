#!/bin/sh
#
# VM Screen Config
# xrandr --output Virtual-1 --mode 1920x1080 &

xrdb merge ~/.Xresources &
systemctl --user import-environment DISPLAY &
# Start WM 
exec qtile start
# exec xtrace qtile start >> ~/.qtile.log
