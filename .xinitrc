#!/bin/sh
#
# VM Screen Config
# xrandr --output Virtual-1 --mode 1920x1080 &

xrdb merge ~/.Xresources &

# Start WM 
exec qtile start
