#!/bin/sh

id=21

pkill wal
xmonad --recompile
if test $? -eq 0; then
    dunstify -a "xmonad" -u low -r $id "compile SUCCESS"
else
    dunstify -a "xmonad" -u high -r $id "compile FAILED"
    exit 1
fi

sh ~/scripts/x11.sh
