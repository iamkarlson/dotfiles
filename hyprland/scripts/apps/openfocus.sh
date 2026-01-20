#!/usr/bin/env zsh

logfile="/tmp/openfocus_error.log"
echo "Openfocus run directory: $(pwd)" >> $logfile

BINARY=$1
SEARCH_TERM=${1:l} # Lowercase for case-insensitive matching

# Function to find the app's class (title or app_id equivalent in Hyprland)
find_app_class() {
    APP_CLASS=$(hyprctl clients -j | jq -r '.[] | select(.class | ascii_downcase | contains("'$SEARCH_TERM'")) | .class' | head -1)
    echo $APP_CLASS
}
# Function to focus on the application, retrying a few times if it does not succeed initially
focus_or_run() {
    for attempt in {1..5}; do
        APP_CLASS=$(find_app_class)
        echo "attempt $attempt and app class is $APP_CLASS"| tee  -a $logfile

        if [[ -n "$APP_CLASS" ]]; then

            hyprctl dispatch focuswindow class:$APP_CLASS && return
        else
            if [[ $attempt -eq 1 ]]; then
                # Try to find and run the binary only on the first attempt
                BINARY_PATH=$(find /usr/bin -name "$BINARY*" -executable | head -1)
                echo "Found binary path: $BINARY_PATH"| tee  -a $logfile
                if [[ -n "$BINARY_PATH" ]]; then
                    nohup "$BINARY_PATH" >/dev/null 2>&1 &
                    sleep 2
                else
                    echo "Error: Application binary not found."| tee  -a $logfile

                    return 1
                fi
            fi
        fi
    done

    echo "Error: Failed to focus or start the application after several attempts." | tee  -a $logfile
    return 1
}

# Main
focus_or_run
echo "Finished openfocus script." >> $logfile
