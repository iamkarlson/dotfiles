#!/bin/bash
# ~/scripts/monitor-setup.sh

################################################################################
# This script can be triggered manually
# or used with a udev rule to automatically to rearrange workspaces when monitor is connected
# Here's an example udev rule:
# ACTION=="change", KERNEL=="card[0-9]*", SUBSYSTEM=="drm", ENV{HOTPLUG}=="1", RUN+="/bin/systemctl --user -M iamkarlson@ start move_workspaces_to_monitors.service"
################################################################################

echo "$(date): Script triggered"

monitors=$(hyprctl monitors --instance 0 -j | jq -r '.[].name')
laptop_monitor="eDP-1"
external_monitor=""


monitor_count=$(hyprctl monitors --instance 0 -j | jq '. | length')

echo "Total monitors detected: $monitor_count"
# Exit early if only one monitor
if [ "$monitor_count" -le 1 ]; then
    echo "Only one monitor detected, skipping workspace rearrangement"
    exit 0
fi

for monitor in $monitors; do
    if [ "$monitor" != "$laptop_monitor" ]; then
        external_monitor="$monitor"
        break
    fi
done

if [ -n "$external_monitor" ]; then
    notify-send "Monitor connected" "External monitor detected: $external_monitor. Workspaces will be moved accordingly."

    for i in {1..10}; do
        hyprctl dispatch moveworkspacetomonitor $i $external_monitor
    done

    for i in {11..20}; do
        hyprctl dispatch moveworkspacetomonitor $i $laptop_monitor
    done
fi
