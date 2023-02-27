#!/usr/bin/env sh
#
IMG_FILE=~/pictures/screenshots/$(date +"%Y%m%d%H%M%S").png;
grim -g "$(slurp)" $IMG_FILE;
$(wl-copy < $IMG_FILE);
