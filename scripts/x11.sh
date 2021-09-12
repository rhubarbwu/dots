#!/bin/sh

restart() {
    case "$(pidof $1 | wc -w)" in
    0) $1 & ;;
    *) ;;
    esac
}

pkill wal

xset led 3
wal -i ~/Wallpapers/*beach*
sh ~/scripts/colors.sh
xrdb -merge ~/.Xresources
restart dunst
restart stalonetray
pkill xmobar
xmonad --restart

