#!/bin/bash

# Script to add [WAYDROID] prefix to Waydroid app names in desktop files
# This prevents confusion between native and Android apps in rofi

WAYDROID_APPS_DIR="$HOME/.local/share/applications"
PREFIX="[WAYDROID] "
LOG_FILE="$HOME/.local/share/waydroid-prefix.log"

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

prefix_waydroid_files() {
    log "Starting to process Waydroid files in $WAYDROID_APPS_DIR"
    
    # Check if directory exists
    if [ ! -d "$WAYDROID_APPS_DIR" ]; then
        log "ERROR: Directory $WAYDROID_APPS_DIR does not exist"
        return 1
    fi
    
    # Count files first
    file_count=$(find "$WAYDROID_APPS_DIR" -name "waydroid.*.desktop" 2>/dev/null | wc -l)
    log "Found $file_count waydroid desktop files"
    
    if [ "$file_count" -eq 0 ]; then
        log "No waydroid desktop files found"
        return 0
    fi
    
    # Find all waydroid desktop files
    find "$WAYDROID_APPS_DIR" -name "waydroid.*.desktop" 2>/dev/null | while read -r desktop_file; do
        log "Processing: $desktop_file"
        
        if [ ! -f "$desktop_file" ]; then
            log "ERROR: File $desktop_file does not exist"
            continue
        fi
        
        # Check if the Name line already has the prefix
        if grep -q "^Name=\[WAYDROID\]" "$desktop_file"; then
            log "SKIP: $(basename "$desktop_file") already has prefix"
            continue
        fi
        
        # Get the current name (only from [Desktop Entry] section, not actions)
        current_name=$(awk '/^\[Desktop Entry\]/{flag=1; next} /^\[/{flag=0} flag && /^Name=/{sub(/^Name=/, ""); print; exit}' "$desktop_file")
        log "Current name in $(basename "$desktop_file"): '$current_name'"
        
        if [ -n "$current_name" ]; then
            # Add prefix to the name
            new_name="${PREFIX}${current_name}"
            
            # Replace the Name line only in [Desktop Entry] section
            if awk -v new_name="$new_name" '
                /^\[Desktop Entry\]/{flag=1}
                /^\[/{if($0!="[Desktop Entry]") flag=0}
                flag && /^Name=/{$0="Name="new_name}
                {print}
            ' "$desktop_file" > "${desktop_file}.tmp" && mv "${desktop_file}.tmp" "$desktop_file"; then
                log "SUCCESS: Updated $(basename "$desktop_file") -> '$new_name'"
            else
                log "ERROR: Failed to update $(basename "$desktop_file")"
                rm -f "${desktop_file}.tmp" 2>/dev/null
            fi
        else
            log "WARNING: No Name field found in $(basename "$desktop_file")"
        fi
    done
    
    log "Finished processing Waydroid files"
}

# If run with --watch flag, monitor for changes
if [[ "$1" == "--watch" ]]; then
    log "Starting watch mode for $WAYDROID_APPS_DIR"
    log "Press Ctrl+C to stop"
    
    # Initial run
    prefix_waydroid_files
    
    # Check if inotifywait is available
    if ! command -v inotifywait &> /dev/null; then
        log "ERROR: inotifywait not found. Install inotify-tools package."
        exit 1
    fi
    
    log "Starting inotifywait monitoring..."
    # Monitor directory for changes
    inotifywait -m -e create,modify,moved_to "$WAYDROID_APPS_DIR" --format '%f' 2>/dev/null | while read filename; do
        if [[ "$filename" == waydroid.*.desktop ]]; then
            log "Detected change: $filename"
            sleep 0.5  # Give waydroid time to finish writing
            prefix_waydroid_files
        fi
    done
else
    log "Running single execution mode"
    prefix_waydroid_files
    log "Done! Run with --watch to monitor for changes."
fi