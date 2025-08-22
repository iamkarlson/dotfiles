#!/usr/bin/env zsh
# Smart terminal launcher - opens terminal with context-aware directory selection

SUPPORTED_APPS=(
    "emacs"
    "Alacritty" 
    "thunar"
    # "Code"  # Disabled - shitty VSCode doesn't set CWD to workspace
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
    if [[ "$app_name" == "alacritty" ]]; then
        target_pid=$(pgrep -P "$app_pid" | head -1)
        echo "$(date): Shell PID: '$target_pid'" >> "$LOGFILE"
    fi
    
    local app_cwd=$(readlink -f "/proc/$target_pid/cwd" 2>/dev/null)
    echo "$(date): $app_name CWD: '$app_cwd'" >> "$LOGFILE"
    
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
    
    RAW_OUTPUT=$(emacsclient -e '
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
    
    EMACS_DIR=$(echo "$RAW_OUTPUT" | tr -d '"')
    echo "$(date): Emacs directory result: '$EMACS_DIR'" >> "$LOGFILE"
    
    if [[ -z "$EMACS_DIR" || "$EMACS_DIR" == "nil" ]]; then
        EMACS_DIR="$HOME"
        echo "$(date): Using fallback: $EMACS_DIR" >> "$LOGFILE"
    fi
    
    echo "$(date): Launching alacritty with --working-directory '$EMACS_DIR'" >> "$LOGFILE"
    alacritty --working-directory "$EMACS_DIR" &

elif (( ${SUPPORTED_APPS[(Ie)$CURRENT_WINDOW]} )); then
    APP_PID=$(hyprctl -j activewindow | jq -r '.pid')
    launch_from_process_cwd "${CURRENT_WINDOW:l}" "$APP_PID"

else
    echo "$(date): Launching terminal normally" >> "$LOGFILE"
    alacritty &
fi
