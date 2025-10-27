#!/bin/bash
#
# Switch to tv and running steam
#

/home/iamkarlson/.config/hypr/scripts/monitors/switch-to-tv.sh

/home/iamkarlson/.config/hypr/scripts/apps/openfocus.sh steam

swaync-client --close-all
swaync-client --dnd-on
swaync-client --close-panel

notify-send.sh -u critical "Gaming Mode Enabled" "Switched to TV and launched Steam."
