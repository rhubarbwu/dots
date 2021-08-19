#!/bin/sh

pkill wal
xset led 3
#wal -i "$(shuf -n1 -e /home/rusbridger/Wallpapers/*)"
wal -i ~/Wallpapers/*aloe*
xrdb -merge ~/.Xresources
xmonad --restart
