#!/bin/sh

pkill dunst
pkill stalonetray
pkill nm-applet

if test $# -gt 0 && test $1 = "-r"; then
    dunst &
    stalonetray &
    nm-applet &
fi
