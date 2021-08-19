#!/bin/sh

pkill wal

xset led 3
#wal -i "$(shuf -n1 -e ~/Wallpapers/*)"
wal -i ~/Wallpapers/*aloe*
xrdb -merge ~/.Xresources
pkill xmobar
xmonad --restart
