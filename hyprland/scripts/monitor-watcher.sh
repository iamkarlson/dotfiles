#!/bin/bash
################################################################################
# monitor-watcher.sh — Listen for Hyprland monitor removal events
#
# Uses Hyprland's IPC socket to detect when a monitor is disconnected,
# then re-runs arrange-workspaces.sh to handle the new layout.
# This covers the "undock while lid closed" scenario.
#
# Started by: host config (e.g. beryl.conf) via exec-once
################################################################################

set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
SOCKET="$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock"

if [ ! -S "$SOCKET" ]; then
    echo "Hyprland IPC socket not found: $SOCKET"
    exit 1
fi

socat -u UNIX-CONNECT:"$SOCKET" - | while read -r event; do
    case "$event" in
        monitorremoved\>\>*)
            echo "$(date '+%Y-%m-%d %H:%M:%S') [monitor-watcher] Monitor removed: ${event#*>>}"
            sleep 1
            "$SCRIPT_DIR/arrange-workspaces.sh"
            ;;
    esac
done
