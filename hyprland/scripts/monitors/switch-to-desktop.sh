#!/bin/bash
# Switch to desktop monitor (HDMI-A-1) and disable TV (HDMI-A-2)

# Enable desktop and disable TV
hyprctl keyword monitor "HDMI-A-1,preferred,auto,1"
hyprctl keyword monitor "HDMI-A-2,disable"

# Move all workspaces to desktop
for workspace in $(hyprctl workspaces -j | jq -r '.[].id'); do
    hyprctl dispatch moveworkspacetomonitor "$workspace" HDMI-A-1
done
