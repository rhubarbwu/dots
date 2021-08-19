#! /bin/bash

xmodmap ~/.config/X11/Xmodmap
sh ~/scripts/x11.sh
dunst &
stalonetray &
nm-applet &
exec xmonad
