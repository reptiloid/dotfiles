#!/bin/sh
#
# VM Screen Config
# xrandr --output Virtual1 --mode 1440x900 &

xrdb merge ~/.Xresources &

# Start WM 
exec qtile start
