#!/usr/bin/env sh
#

IMG_FILE=~/Pictures/screenshots/$(date +"%Y%m%d%H%M%S").png;
grim -o "$(swaymsg -t get_outputs | jq -r '.[] | select(.focused) | .name')" $IMG_FILE;
$(wl-copy < $IMG_FILE);
