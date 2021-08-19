#!/bin/sh

pkill xmobar
active=$(xrandr --listmonitors | grep " +")
echo "$active" | while read -r "a"; do
    index=$(echo $a | head -c 1)
    xmobar -x $index ~/.xmonad/xmobarrc.hs &
done
