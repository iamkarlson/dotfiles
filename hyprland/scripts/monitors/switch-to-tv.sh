#!/bin/bash
# Switch to TV (HDMI-A-2) and disable desktop monitor (HDMI-A-1)

# Enable TV and disable desktop
#hyprctl keyword monitor "HDMI-A-2,preferred,auto,1"
hyprctl keyword monitor "HDMI-A-2,3840x2160@60.00Hz,auto,2"
hyprctl keyword monitor "HDMI-A-1,disable"

# Move all workspaces to TV
for workspace in $(hyprctl workspaces -j | jq -r '.[].id'); do
    hyprctl dispatch moveworkspacetomonitor "$workspace" HDMI-A-2
done
