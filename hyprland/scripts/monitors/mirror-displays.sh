#!/bin/bash
# Mirror both displays (HDMI-A-1 and HDMI-A-2)

# Enable both monitors with mirroring
hyprctl keyword monitor "HDMI-A-1,preferred,auto,1,mirror,HDMI-A-2"
hyprctl keyword monitor "HDMI-A-2,preferred,auto,1"

# Move all workspaces to desktop monitor
for workspace in $(hyprctl workspaces -j | jq -r '.[].id'); do
    hyprctl dispatch moveworkspacetomonitor "$workspace" HDMI-A-1
done
