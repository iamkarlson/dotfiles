#!/bin/bash
# ~/scripts/monitor-setup.sh

current_monitor=$(hyprctl activeworkspace -j | jq -r .monitor)
echo "Current monitor: $current_monitor"

monitors=$(hyprctl monitors -j | jq -r '.[].name')

for monitor in $monitors; do
    echo "Checking monitor: $monitor"
    if [ "$monitor" != "$current_monitor" ]; then
        echo "Found next monitor: $monitor"
        next_monitor="$monitor"
        break
    fi
done

echo "Moving current workspace from $current_monitor to $next_monitor"

hyprctl dispatch movecurrentworkspacetomonitor $next_monitor
