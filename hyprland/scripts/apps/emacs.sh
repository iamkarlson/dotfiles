#!/usr/bin/env zsh
# Script for jumping to Emacs

FILE=$1

# Check if Emacs window exists using hyprctl and jq
EMACS_WINDOW=$(hyprctl -j clients | jq -r '.[] | select(.class == "emacs") | .address')

if [[ -n $EMACS_WINDOW ]]; then
    if [[ -z $FILE ]]; then
        # No file specified, just switch focus to the existing Emacs window
        notify-send.sh "Emacs is open and no file!";
        hyprctl dispatch focuswindow address:$EMACS_WINDOW >/dev/null
    else
        # File specified, open it in Emacs and focus on it
        notify-send.sh "Emacs is open and file is passed!";
        emacsclient -r -a "" -n "$FILE" >/dev/null 2>/dev/null &
        sleep 0.1
        hyprctl dispatch focuswindow address:$EMACS_WINDOW >/dev/null
    fi
else

    if [[ -z $FILE ]]; then
        # No file specified, just switch focus to the existing Emacs window
        notify-send.sh "Emacs is not found and no file!";
        emacsclient -r -a "" -n >/dev/null 2>/dev/null &
        hyprctl dispatch focuswindow address:$EMACS_WINDOW >/dev/null
    else
        # Emacs window not found, start Emacs with specified file if provided
        notify-send.sh "Emacs is not found and file is passed!";
        emacsclient -r -a "" -n $FILE >/dev/null 2>/dev/null &
        sleep 0.1
        hyprctl dispatch focuswindow emacs >/dev/null
    fi
fi
