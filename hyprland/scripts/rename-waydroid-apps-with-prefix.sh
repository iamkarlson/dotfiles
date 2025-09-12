#!/bin/bash

# Script to add [WAYDROID] prefix to Waydroid app names in desktop files
# This prevents confusion between native and Android apps in rofi

WAYDROID_APPS_DIR="$HOME/.local/share/applications"
PREFIX="[WAYDROID] "

prefix_waydroid_files() {
    # Find all waydroid desktop files
    find "$WAYDROID_APPS_DIR" -name "waydroid.*.desktop" | while read -r desktop_file; do
        # Check if the Name line already has the prefix
        if grep -q "^Name=\[WAYDROID\]" "$desktop_file"; then
            continue
        fi
        
        # Get the current name
        current_name=$(grep "^Name=" "$desktop_file" | cut -d'=' -f2-)
        
        if [ -n "$current_name" ]; then
            # Add prefix to the name
            new_name="${PREFIX}${current_name}"
            
            # Replace the Name line
            sed -i "s/^Name=.*/Name=$new_name/" "$desktop_file"
            
            echo "Updated: $(basename "$desktop_file") -> '$new_name'"
        fi
    done
}

# If run with --watch flag, monitor for changes
if [[ "$1" == "--watch" ]]; then
    echo "Monitoring $WAYDROID_APPS_DIR for Waydroid app changes..."
    echo "Press Ctrl+C to stop"
    
    # Initial run
    prefix_waydroid_files
    
    # Monitor directory for changes
    inotifywait -m -e create,modify,moved_to "$WAYDROID_APPS_DIR" --format '%f' | while read filename; do
        if [[ "$filename" == waydroid.*.desktop ]]; then
            echo "Detected change: $filename"
            sleep 0.5  # Give waydroid time to finish writing
            prefix_waydroid_files
        fi
    done
else
    echo "Adding [WAYDROID] prefix to Waydroid app names..."
    prefix_waydroid_files
    echo "Done! Run with --watch to monitor for changes."
fi