#!/usr/bin/env bash
# Enforce workspace lock for specific applications
# Usage: enforce_workspace_lock.sh <window_address> <class_pattern> <target_workspace>
#
# Called by hyprwhenthen on movewindow events to force certain apps back to their assigned workspace

WINDOW_ADDR="$1"
CLASS_PATTERN="$2"
TARGET_WS="$3"

[[ -z "$WINDOW_ADDR" || -z "$CLASS_PATTERN" || -z "$TARGET_WS" ]] && exit 0

# Get the window class for this address
WINDOW_CLASS=$(hyprctl clients -j | jq -r --arg addr "0x$WINDOW_ADDR" '.[] | select(.address == $addr) | .class')

[[ -z "$WINDOW_CLASS" ]] && exit 0

# Check if the window class matches the locked pattern
if [[ "$WINDOW_CLASS" =~ $CLASS_PATTERN ]]; then
    # Get current workspace of the window
    CURRENT_WS=$(hyprctl clients -j | jq -r --arg addr "0x$WINDOW_ADDR" '.[] | select(.address == $addr) | .workspace.name')

    # If not on target workspace, move it back
    if [[ "$CURRENT_WS" != "$TARGET_WS" ]]; then
        hyprctl dispatch movetoworkspacesilent "$TARGET_WS,address:0x$WINDOW_ADDR"
    fi
fi
