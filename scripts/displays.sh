#!/bin/sh

case $1 in
"-a")
    pkill arandr
    arandr
    sh ~/scripts/x11.sh
    ;;
"-d") autorandr --load mobile ;;
"-t")
    pkill arandr
    monitors=$(xrandr | grep " connected")
    selected=$(echo -e "$monitors" | rofi -lines 3 -dmenu -p "xrandr toggle" | awk '{print $1;}')
    if test "$selected" = ""; then
	exit 0
    fi
    active=$(xrandr --listmonitors | grep " +")
    if test "${active#*$selected}" != "$active"; then
        xrandr --output $selected --off
    else
        xrandr --output $selected --auto
    fi
    arandr
    ;;
*) ;;
esac

sh ~/scripts/x11.sh
sh ~/scripts/xmobar.sh
