#!/bin/bash
set -o nounset  # exit on uninitialised variable
set -o errexit  # exit on error
#set -o xtrace   # debug mode

# setxkbmap -option grp:ralt_rshift_toggle us,ru &
setxkbmap -option grp:caps_toggle us,ru &

xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-0 --mode 1920x1080 --pos 1920x0 --rotate normal &

picom &
opensnitch-ui &
emacs --daemon &
feh --bg-fill ~/Pictures/walls/3.jpg &
# feh --bg-fill -z ~/Pictures/walls
