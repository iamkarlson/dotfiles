#!/usr/bin/env sh

swaymsg "output * dpms off"

brightnessctl --device='tpacpi::kbd_backlight' set 0
