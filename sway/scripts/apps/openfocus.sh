#!/usr/bin/env zsh

# Enhanced Script for focusing or running applications in swaywm

BINARY=$1
# Convert to lowercase for matching purposes
SEARCH_TERM=${1:l}

# Function to find the app_id from swaywm's tree
find_app_id() {
    APP_ID=$(swaymsg -t get_tree | jq -r '.. | select(.app_id? != null) | .app_id | ascii_downcase | select(contains("'$SEARCH_TERM'"))' | head -1)
    echo $APP_ID
}

focus_or_run() {
    # Find the app_id that matches our SEARCH_TERM
    APP_ID=$(find_app_id)

    if [[ -n "$APP_ID" ]]; then
        # Focus if the app is found
        swaymsg "[app_id=\"$APP_ID\"] focus"
    else
        # Try to find the binary in /usr/bin and execute it
        BINARY_PATH=$(find /usr/bin -name "$BINARY*" | head -1)
        if [[ -n "$BINARY_PATH" ]]; then
            nohup "$BINARY_PATH" >/dev/null 2>&1 &
            sleep 1 # Give it a second to open
            # Focus after giving time to open
            APP_ID=$(find_app_id)
            if [[ -n "$APP_ID" ]]; then
                swaymsg "[app_id=\"$APP_ID\"] focus"
            fi
        else
            echo "Error: Application not found."
        fi
    fi
}

# Main
focus_or_run
