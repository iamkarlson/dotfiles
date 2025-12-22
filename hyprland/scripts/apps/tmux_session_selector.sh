#!/usr/bin/env zsh
# Tmux session selector - opens alacritty and attaches to selected tmux session

LOGFILE="/tmp/tmux_session_selector.log"

echo "$(date): Starting tmux session selector" >> "$LOGFILE"

# Get list of existing tmux sessions with their current working directory
# Format: "session_name | /path/to/directory"
SESSION_LIST=""
while IFS= read -r session; do
    # Get the current working directory of the active pane in this session
    cwd=$(tmux display-message -t "$session" -p '#{pane_current_path}' 2>/dev/null || echo "~")
    # Shorten home directory to ~
    cwd=${cwd/#$HOME/\~}
    SESSION_LIST+="$session | $cwd"$'\n'
done < <(tmux list-sessions -F "#{session_name}" 2>/dev/null)

if [[ -z "$SESSION_LIST" ]]; then
    echo "$(date): No tmux sessions found, launching terminal normally" >> "$LOGFILE"
    notify-send "Tmux Session Selector" "No existing sessions found" -i utilities-terminal
    alacritty &
    exit 0
fi

echo "$(date): Found sessions with directories: $SESSION_LIST" >> "$LOGFILE"

# Use rofi to select a session (or fallback to fzf in terminal)
if command -v rofi &> /dev/null; then
    SELECTED=$(echo "$SESSION_LIST" | rofi -dmenu -i -p "Select tmux session")
    echo "$(date): Selected via rofi: '$SELECTED'" >> "$LOGFILE"
else
    # Fallback to alacritty with fzf
    echo "$(date): Rofi not found, falling back to fzf" >> "$LOGFILE"
    SELECTED=$(echo "$SESSION_LIST" | alacritty --class "tmux-selector" --title "Select Tmux Session" -e zsh -c "echo '$SESSION_LIST' | fzf --prompt='Select tmux session: ' --height=40% --reverse")
    echo "$(date): Selected via fzf: '$SELECTED'" >> "$LOGFILE"
fi

# If no session was selected (user cancelled), exit
if [[ -z "$SELECTED" ]]; then
    echo "$(date): No session selected, exiting" >> "$LOGFILE"
    exit 0
fi

# Extract just the session name (before the pipe)
SELECTED_SESSION=$(echo "$SELECTED" | cut -d'|' -f1 | sed 's/[[:space:]]*$//')

# Launch alacritty and attach to the selected session
echo "$(date): Launching alacritty with tmux attach to '$SELECTED_SESSION'" >> "$LOGFILE"
alacritty -e tmux attach -t "$SELECTED_SESSION" &

echo "$(date): Done" >> "$LOGFILE"
