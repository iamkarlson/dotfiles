#!/bin/bash
################################################################################
# arrange-workspaces.sh — Dynamic monitor & workspace configuration
#
# Detects lid state and connected monitors, then arranges workspaces:
#   - Docked (lid closed + external):  all WS on external, eDP-1 disabled
#   - Office (lid open + external):    WS 1-10 on external, WS 11-20 on eDP-1
#   - Laptop-only (lid open, no ext):  all WS on eDP-1
#   - Lid closed + no external:        do nothing (logind handles suspend)
#
# Triggered by: lid switch events, monitor hotplug, startup, config reload
################################################################################

set -euo pipefail

LAPTOP_MONITOR="eDP-1"
LAPTOP_RES="1920x1200@60"

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') [arrange-workspaces] $*"
}

get_lid_state() {
    local state
    state=$(cat /proc/acpi/button/lid/LID0/state 2>/dev/null \
         || cat /proc/acpi/button/lid/LID/state 2>/dev/null \
         || echo "state: open")
    echo "$state" | awk '{print $2}'
}

# Resolve resolution for a known external monitor based on its description.
# Falls back to "preferred" for unknown monitors.
get_external_resolution() {
    local desc="$1"
    case "$desc" in
        *"LG HDR WQHD"*)
            echo "3440x1440@49.99"
            ;;
        *)
            echo "preferred"
            ;;
    esac
}

# Query Hyprland for connected monitors
monitors_json=$(hyprctl monitors -j 2>/dev/null) || {
    log "hyprctl not available or Hyprland not running"
    exit 1
}

lid_state=$(get_lid_state)
external=$(echo "$monitors_json" | jq -r ".[].name | select(. != \"$LAPTOP_MONITOR\")" | head -1)
external_desc=$(echo "$monitors_json" | jq -r ".[] | select(.name != \"$LAPTOP_MONITOR\") | .description" | head -1)
laptop_active=$(echo "$monitors_json" | jq -r ".[].name | select(. == \"$LAPTOP_MONITOR\")" | head -1)
external_res=$(get_external_resolution "${external_desc:-}")

log "lid=$lid_state external=${external:-none} (${external_desc:-}) laptop_active=${laptop_active:-none} res=$external_res"

move_all_to() {
    local target="$1"
    local batch=""
    for i in $(seq 1 20); do
        batch+="dispatch moveworkspacetomonitor $i $target;"
    done
    hyprctl --batch "$batch"
}

if [ "$lid_state" = "closed" ] && [ -n "$external" ]; then
    # DOCKED: lid closed, external present
    # External at 0x0, laptop disabled
    log "Docked mode: disabling $LAPTOP_MONITOR, all workspaces to $external"
    hyprctl keyword monitor "$LAPTOP_MONITOR,disable"
    hyprctl keyword monitor "$external,$external_res,0x0,1"
    move_all_to "$external"
    notify-send -a "Display" "Docked mode" "All workspaces on $external ($external_res)"

elif [ "$lid_state" = "open" ] && [ -n "$external" ]; then
    # OFFICE: lid open, external present
    # Laptop at 0x0 (left), external to its right
    if [ -z "$laptop_active" ]; then
        log "Re-enabling $LAPTOP_MONITOR"
        hyprctl keyword monitor "$LAPTOP_MONITOR,$LAPTOP_RES,0x0,1"
        sleep 0.5
    else
        hyprctl keyword monitor "$LAPTOP_MONITOR,$LAPTOP_RES,0x0,1"
    fi
    hyprctl keyword monitor "$external,$external_res,1920x0,1"

    log "Office mode: WS 1-10 on $external, WS 11-20 on $LAPTOP_MONITOR"
    batch=""
    for i in $(seq 1 10); do
        batch+="dispatch moveworkspacetomonitor $i $external;"
    done
    for i in $(seq 11 20); do
        batch+="dispatch moveworkspacetomonitor $i $LAPTOP_MONITOR;"
    done
    hyprctl --batch "$batch"
    notify-send -a "Display" "Office mode" "WS 1-10 on $external ($external_res), WS 11-20 on $LAPTOP_MONITOR"

elif [ "$lid_state" = "open" ] && [ -z "$external" ]; then
    # LAPTOP ONLY: no external monitor
    if [ -z "$laptop_active" ]; then
        log "Re-enabling $LAPTOP_MONITOR"
        hyprctl keyword monitor "$LAPTOP_MONITOR,$LAPTOP_RES,0x0,1"
        sleep 0.5
    fi

    log "Laptop-only mode: all workspaces on $LAPTOP_MONITOR"
    move_all_to "$LAPTOP_MONITOR"
    notify-send -a "Display" "Laptop only" "All workspaces on $LAPTOP_MONITOR"

else
    # LID CLOSED + NO EXTERNAL: lock and suspend
    # This handles the undocking case (cable disconnected while lid was closed)
    log "Lid closed, no external monitor — locking and suspending"
    hyprlock --immediate &
    sleep 1
    systemctl suspend
fi
