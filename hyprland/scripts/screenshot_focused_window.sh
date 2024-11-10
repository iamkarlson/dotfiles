#!/usr/bin/env sh
#

FILE_NAME=$(date +"%Y%m%d%H%M%S").png;
IMG_FILE=~/pictures/screenshots/$FILE_NAME;
grim -o $(hyprctl -j monitors | jq -r '.[] | select(.focused == true) | .name') $IMG_FILE;
$(wl-copy < $IMG_FILE);


notify-send.sh "Screenshot is made!" "$FILE_NAME copied to clipboard!" -i "$IMG_FILE" -d "xdg-open $IMG_FILE";
