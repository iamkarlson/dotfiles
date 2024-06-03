#!/bin/bash

# Directory with wallpapers
WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"

if [ -z "$WALLPAPERS_DIR" ]; then
  echo "WALLPAPERS_DIR is not set."
  echo "wallpapers dir does not exist - using fallback"
  WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"
elif [ -d $WALLPAPERS_DIR ]; then
  echo "wallpapers dir exists"
  #ls $WALLPAPERS_DIR

fi

#WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"

echo "wallpapers dir"
echo $WALLPAPERS_DIR

# File to store the current wallpaper
CURRENT_WALLPAPER_FILE="$HOME/.current_wallpaper"

# Get list of monitors
MONITORS=$(swaymsg -t get_outputs | jq -r '.[].name')

# Function to get next wallpaper
get_next_wallpaper() {
  local last_wallpaper="$1"
  local wallpapers=("$WALLPAPERS_DIR"/*)

  if [ -z "$last_wallpaper" ]; then
    # Pick a random wallpaper if no last wallpaper is set
    echo "${wallpapers[RANDOM % ${#wallpapers[@]}]}"
  else
    # Find the index of the last wallpaper
    local index=-1
    for i in "${!wallpapers[@]}"; do
      if [ "${wallpapers[$i]}" == "$last_wallpaper" ]; then
        index=$i
        break
      fi
    done

    # Get the next wallpaper in the array
    echo "${wallpapers[((index + 1) % ${#wallpapers[@]})]}"
  fi
}

# Check if .current_wallpaper file exists
if [ -f "$CURRENT_WALLPAPER_FILE" ]; then
  LAST_WALLPAPER=$(cat "$CURRENT_WALLPAPER_FILE")
else
  LAST_WALLPAPER=""
fi

# Iterate over each monitor and set wallpaper
for MONITOR in $MONITORS; do
  NEW_WALLPAPER=$(get_next_wallpaper "$LAST_WALLPAPER")

  swww img -o "$MONITOR" "$NEW_WALLPAPER"
  echo "$NEW_WALLPAPER" > "$CURRENT_WALLPAPER_FILE"
  LAST_WALLPAPER="$NEW_WALLPAPER"
done

