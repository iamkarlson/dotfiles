#!/usr/bin/env zsh

BINARY=$1
SEARCH_TERM=${1:l} # Lowercase for case-insensitive matching

# Function to find the app_id from swaywm's tree
find_app_id() {
    APP_ID=$(swaymsg -t get_tree | jq -r '.. | select(.app_id? != null) | .app_id | ascii_downcase | select(contains("'$SEARCH_TERM'"))' | head -1)
    echo $APP_ID
}

# Function to focus on the application, retrying a few times if it does not succeed initially
focus_or_run() {
    for attempt in {1..5}; do
        APP_ID=$(find_app_id)
        echo "attempt $attempt and app id is $APP_ID"

        if [[ -n "$APP_ID" ]]; then
            swaymsg "[app_id=\"$APP_ID\"] focus" && return
        else
            if [[ $attempt -eq 1 ]]; then
                # Try to find and run the binary only on the first attempt
                BINARY_PATH=$(find /usr/bin -name "$BINARY*" -executable | head -1)
                if [[ -n "$BINARY_PATH" ]]; then
                    nohup "$BINARY_PATH" >/dev/null 2>&1 &
                    sleep 2
                else
                    echo "Error: Application binary not found."
                    return 1
                fi
            fi
        fi
    done

    echo "Error: Failed to focus or start the application after several attempts."
    return 1
}

# Main
focus_or_run
