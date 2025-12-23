#!/usr/bin/env zsh
# Smart terminal launcher - opens terminal with context-aware directory selection

SUPPORTED_APPS=(
    "emacs"
    "Alacritty" 
    "thunar"
    "pycharm"
    "datagrip"
)

LOGFILE="/tmp/smart_terminal.log"

launch_from_process_cwd() {
    local app_name="$1"
    local app_pid="$2"
    
    echo "$(date): Launching terminal from $app_name" >> "$LOGFILE"
    echo "$(date): $app_name PID: '$app_pid'" >> "$LOGFILE"
    
    # For Alacritty, get shell child process CWD; for others, use app's own CWD
    local target_pid="$app_pid"
    local app_cwd=""

    if [[ "$app_name" == "alacritty" ]]; then
        # Get first child (might be tmux or shell directly)
        local child_pid=$(pgrep -P "$app_pid" | head -1)
        echo "$(date): First child PID: '$child_pid'" >> "$LOGFILE"

        # Check if the child is tmux
        local process_name=$(ps -p "$child_pid" -o comm= 2>/dev/null)
        if [[ "$process_name" == "tmux"* ]]; then
            echo "$(date): Detected tmux, using tmux to get current directory" >> "$LOGFILE"
            # Use tmux to get the current pane's working directory
            app_cwd=$(tmux display-message -p '#{pane_current_path}' 2>/dev/null)
            echo "$(date): Tmux pane CWD: '$app_cwd'" >> "$LOGFILE"
        else
            # Direct shell, use its CWD
            target_pid="$child_pid"
            app_cwd=$(readlink -f "/proc/$target_pid/cwd" 2>/dev/null)
            echo "$(date): Shell PID: '$target_pid'" >> "$LOGFILE"
            echo "$(date): Shell CWD: '$app_cwd'" >> "$LOGFILE"
        fi
    else
        app_cwd=$(readlink -f "/proc/$target_pid/cwd" 2>/dev/null)
        echo "$(date): $app_name CWD: '$app_cwd'" >> "$LOGFILE"
    fi
    
    if [[ -n "$app_cwd" && -d "$app_cwd" ]]; then
        alacritty --working-directory "$app_cwd" &
        echo "$(date): Launched in $app_name directory: '$app_cwd'" >> "$LOGFILE"
    else
        alacritty &
        echo "$(date): $app_name CWD not found, launched normally" >> "$LOGFILE"
    fi
}

CURRENT_WINDOW=$(hyprctl -j activewindow | jq -r '.class')

echo "$(date): Current window class: '$CURRENT_WINDOW'" >> "$LOGFILE"

if [[ "$CURRENT_WINDOW" == "emacs" ]]; then
    echo "$(date): Launching terminal from emacs" >> "$LOGFILE"
    
    # Get directory using combined elisp logic with timeout
    RAW_OUTPUT=$(timeout 1s emacsclient -e '
    (if (and (featurep (quote projectile)) (projectile-project-p))
        (projectile-project-root)
      (if (featurep (quote projectile))
          (let ((file-buffer (cl-find-if (lambda (buf) 
                                           (buffer-file-name buf))
                                         (buffer-list))))
            (if file-buffer
                (with-current-buffer file-buffer
                  (if (projectile-project-p)
                      (projectile-project-root)
                    (file-name-directory (buffer-file-name))))
              (expand-file-name default-directory)))
        (expand-file-name default-directory)))' 2>&1)
    
    EMACS_EXIT_CODE=$?
    
    # Check if emacsclient timed out or failed
    if [[ $EMACS_EXIT_CODE -ne 0 ]]; then
        EMACS_DIR="$HOME"
        if [[ $EMACS_EXIT_CODE -eq 124 ]]; then
            echo "$(date): emacsclient timed out, using home directory" >> "$LOGFILE"
            notify-send "Smart Terminal" "emacsclient timed out - using home directory" -i utilities-terminal
        else
            echo "$(date): emacsclient failed (exit $EMACS_EXIT_CODE), using home directory" >> "$LOGFILE"
            notify-send "Smart Terminal" "emacsclient failed - using home directory" -i utilities-terminal
        fi
    else
        EMACS_DIR=$(echo "$RAW_OUTPUT" | tr -d '"')
        echo "$(date): Emacs directory result: '$EMACS_DIR'" >> "$LOGFILE"
        
        # Fallback to home if emacsclient returns nil
        if [[ -z "$EMACS_DIR" || "$EMACS_DIR" == "nil" ]]; then
            EMACS_DIR="$HOME"
            echo "$(date): emacsclient returned nil, using home directory" >> "$LOGFILE"
            notify-send "Smart Terminal" "Could not get Emacs directory - using home directory" -i utilities-terminal
        fi
    fi
    
    echo "$(date): Launching alacritty with --working-directory '$EMACS_DIR'" >> "$LOGFILE"
    alacritty --working-directory "$EMACS_DIR" &

elif [[ "$CURRENT_WINDOW" == "Code" ]] || [[ "$CURRENT_WINDOW" == "code" ]]; then
    echo "$(date): Launching terminal from vscode" >> "$LOGFILE"
    
    # Get VSCode workspace from window title [~/path/to/workspace]
    VSCODE_TITLE=$(hyprctl -j activewindow | jq -r '.title')
    echo "$(date): VSCode title: '$VSCODE_TITLE'" >> "$LOGFILE"
    
    # Extract path from brackets and expand tilde
    VSCODE_PATH=$(echo "$VSCODE_TITLE" | grep -o '\[~/[^]]*\]' | tr -d '[]' | sed "s|^~|$HOME|")
    echo "$(date): Extracted VSCode path: '$VSCODE_PATH'" >> "$LOGFILE"
    
    if [[ -n "$VSCODE_PATH" && -d "$VSCODE_PATH" ]]; then
        echo "$(date): Launching alacritty with --working-directory '$VSCODE_PATH'" >> "$LOGFILE"
        alacritty --working-directory "$VSCODE_PATH" &
    else
        echo "$(date): VSCode path not found or invalid, launching normally" >> "$LOGFILE"
        alacritty &
    fi

elif (( ${SUPPORTED_APPS[(Ie)$CURRENT_WINDOW]} )); then
    APP_PID=$(hyprctl -j activewindow | jq -r '.pid')
    launch_from_process_cwd "${CURRENT_WINDOW:l}" "$APP_PID"

else
    echo "$(date): Launching terminal normally" >> "$LOGFILE"
    alacritty &
fi
