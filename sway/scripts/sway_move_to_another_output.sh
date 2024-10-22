#!/usr/bin/env bash
#
# Script to move the current workspace to the next output
#
swaymsg "move workspace to output $(swaymsg -t get_outputs | jq -r '.[] | select(.focused == false) | .name' | head -n 1)"
