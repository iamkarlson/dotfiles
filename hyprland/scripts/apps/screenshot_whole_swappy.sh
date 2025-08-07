#!/usr/bin/env sh
#
IMG_FILE=~/pictures/screenshots/$(date +"%Y%m%d%H%M%S").png
grim $IMG_FILE
swappy -f $IMG_FILE -o $IMG_FILE
wl-copy < $IMG_FILE
