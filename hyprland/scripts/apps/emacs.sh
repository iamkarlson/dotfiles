#!/usr/bin/env zsh
# Script for jumping to Emacs
#

FILE=$1


emacsclient -r -a "" -n $FILE>/dev/null 2>/dev/null &
sleep 0.1
hyprctl dispatch focuswindow emacs >/dev/null
