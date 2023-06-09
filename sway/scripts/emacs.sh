#!/usr/bin/env sh
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

focus() {
    swaymsg "$FOCUS focus" >/dev/null
}
open_if_not_running() {
    if ! pgrep -fa "/usr/.*/$BINARY" >/dev/null; then
        # When the application is already running in the background,
        # I don't want to do anything
        # If the application is not running, start it but do not wait for it.
        emacsclient -c -a "" -n >/dev/null 2>/dev/null &
    fi
}
focus_wait() {
    # For applications that are slow to start, we try every 0.1s until their
    # window is open.
    for i in {1..30}; do
        if focus; then
            break
        fi
        sleep 0.1
    done
}

focus || (
    open_if_not_running
    focus_wait
)
