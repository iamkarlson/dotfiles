#!/usr/bin/env sh

swaymsg output * dpms on

brightnessctl --device='tpacpi::kbd_backlight' set 2
