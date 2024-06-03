#!/usr/bin/env sh

now=$(date +"%Y-%m-%d_%H-%M-%S")
echo "undim screen $now" >> /tmp/undim.log
swaymsg "output * dpms on"

brightnessctl --device='tpacpi::kbd_backlight' set 2
