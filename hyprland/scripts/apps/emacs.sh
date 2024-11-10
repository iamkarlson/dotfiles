#!/usr/bin/env zsh
# Script for jumping to Emacs
#
#
# Sway tree looks like this for emacs:
#     #39: workspace "3: ‭‬ "
#      #38: con "emacs.sh – Doom Emacs" (xdg_shell, pid: 1654, app_id: "emacs")
# so I will look by app id to focus
#
# Doom desktop file looks like this:
#
#[Desktop Entry]
#Version=1.0
#Type=Application
#Name=Doom Emacs Client
#Exec=emacsclient -c -a "" -n %F
#Icon=emacs
#Categories=Development;TextEditor;
#Terminal=false
#StartupWMClass=Emacs
#GenericName=Text Editor
#Comment=Edit your fucking text
#MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
#StartupNotify=true
#
#

BINARY='emacsclient'
FOCUS='[app_id="emacs"]'
FILE=$1


emacsclient -r -a "" -n $FILE>/dev/null 2>/dev/null &
sleep 0.1
hyprctl dispatch focuswindow emacs >/dev/null
