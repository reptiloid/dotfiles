#!/bin/bash
set -o nounset  # exit on uninitialised variable
set -o errexit  # exit on error
#set -o xtrace   # debug mode
setxkbmap -option grp:ralt_rshift_toggle us,ru &
setxkbmap -option grp:caps_toggle us,ru &

# sxhkd &
picom --transparent-clipping &
opensnitch-ui &
feh --bg-fill ~/Pictures/walls/3.jpg &
# feh --bg-fill -z ~/Pictures/walls
