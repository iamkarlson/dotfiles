
# Smart SSH that replaces current shell with remote tmux
# Use built-in tmux session switcher (ctrl+b s) once connected
ssh-join-tmux() {
  if [[ -z $1 ]]; then
    echo "Usage: ssh-join-tmux <host> [ssh options]"
    return 1
  fi
  echo "Params: $@"
  # If ssh succeeded and you want to close the local shell after, uncomment:
  hyprctl dispatch exec "NO_TMUX=1 alacritty -e zsh -c 'ssh $@'"
}

# Function to create a notification with the notify-send command
# parameter $TIME is the time to wait before sending the notification
ring_the_bell(){
  local TIME=$1
  notify-send \
    --urgency=critical \
    --expire-time=120 \
    --icon=appointment-soon-symbolic \
  "Timer countdown $TIME" "Time's up!"
  echo "timer rang at $(date +'%H:%M:%S')"
}

timer_countdown() {
  # validate input
  #
  #validate that the time parameter is passed. Otherwise, echo an error message and return 1.

  if [[ -z $1 ]]; then
    echo "missing time parameter"
    echo "usage: timer_countdown 2h3m4s"
    return 1
  fi

  if [[ ! $1 =~ ^[0-9]+[hms]?$ ]]; then
    echo "wrong format"
    return 1
  fi

  local total_seconds=0

  if [[ $1 =~ ([0-9]+)h ]]; then
    echo "parsed ${match[1]} hours"
    total_seconds=$((total_seconds + ${match[1]} * 3600))
  fi

  if [[ $1 =~ ([0-9]+)m ]]; then
    echo "parsed ${match[1]} minutes"
    total_seconds=$((total_seconds + ${match[1]} * 60))
  fi

  if [[ $1 =~ ([0-9]+)s ]]; then
    echo "parsed ${match[1]} seconds"
    total_seconds=$((total_seconds + ${match[1]}))
  fi

  if [[ $1 =~ ^([0-9]+)$ ]]; then
    echo "parsed ${match[1]} seconds from number argument"
    total_seconds=$((total_seconds + ${match[1]}))
  fi

  if [[ $total_seconds -eq 0 ]]; then
    echo "no seconds parsed"
    return 1
  fi

  echo "total seconds: $total_seconds"
  {sleep $total_seconds && ring_the_bell $1 }&
}

# Interactive history browser with context (like tig for shell history)
# Usage: hbrowse <search_term> [context_lines]
# Example: hbrowse lsof 10
hbrowse() {
  if [[ -z $1 ]]; then
    echo "Usage: hbrowse <search_term> [context_lines]"
    echo "Example: hbrowse lsof 10"
    return 1
  fi

  local search="$1"
  local context="${2:-5}"  # default 5 lines of context
  local histdb="$HOME/.histdb/zsh-history.db"

  # Preview query - shows context around selected command
  local preview_query="SELECT datetime(h.start_time, 'unixepoch', 'localtime') || '  ' || c.argv || '  ' || CASE WHEN h.id = {1} THEN '<<<' ELSE '' END FROM history h JOIN commands c ON h.command_id = c.id WHERE h.id BETWEEN ({1} - $context) AND ({1} + $context) ORDER BY h.start_time"

  # Get all occurrences of the search term
  local selection=$(sqlite3 "$histdb" << EOF | \
    fzf --height=80% \
        --delimiter=' \| ' \
        --preview="sqlite3 '$histdb' \"$preview_query\"" \
        --preview-window=up:70% \
        --bind='ctrl-/:toggle-preview' \
        --header='Select command occurrence (Ctrl-/ to toggle preview, Enter to show full context)'
SELECT
    h.id || ' | ' ||
    datetime(h.start_time, 'unixepoch', 'localtime') || ' | ' ||
    p.dir || ' | ' ||
    c.argv
FROM history h
JOIN commands c ON h.command_id = c.id
JOIN places p ON h.place_id = p.id
WHERE c.argv LIKE '%${search}%'
ORDER BY h.start_time DESC;
EOF
)

  if [[ -n "$selection" ]]; then
    local target_id=$(echo "$selection" | awk -F' | ' '{print $1}')

    echo "\n=== Context around selected command ==="
    sqlite3 "$histdb" << EOF
.mode column
.headers on
SELECT
    datetime(h.start_time, 'unixepoch', 'localtime') as Time,
    c.argv as Command,
    p.dir as Directory,
    CASE WHEN h.id = $target_id THEN '<<<' ELSE '' END as Marker
FROM history h
JOIN commands c ON h.command_id = c.id
JOIN places p ON h.place_id = p.id
WHERE h.id BETWEEN ($target_id - $context) AND ($target_id + $context)
ORDER BY h.start_time;
EOF
  fi
}

# Search command history with context (N commands before/after)
# Usage: hcontext <search_term> [context_lines]
# Example: hcontext lsof 10
hcontext() {
  if [[ -z $1 ]]; then
    echo "Usage: hcontext <search_term> [context_lines]"
    echo "Example: hcontext lsof 5"
    return 1
  fi

  local search="$1"
  local context="${2:-5}"  # default 5 lines of context

  sqlite3 ~/.histdb/zsh-history.db << EOF
WITH target AS (
    SELECT h.id, h.start_time
    FROM history h
    JOIN commands c ON h.command_id = c.id
    WHERE c.argv LIKE '%${search}%'
    ORDER BY h.start_time DESC
    LIMIT 1
)
SELECT
    datetime(h.start_time, 'unixepoch', 'localtime') as time,
    c.argv,
    p.dir,
    CASE WHEN h.id = target.id THEN ' <<<' ELSE '' END
FROM history h
JOIN commands c ON h.command_id = c.id
JOIN places p ON h.place_id = p.id
CROSS JOIN target
WHERE h.id BETWEEN (target.id - ${context}) AND (target.id + ${context})
ORDER BY h.start_time;
EOF
}

fzf-hyprctl-clients () {
  hyprctl clients -j  | jq -r '.[].title' |fzf | xargs -I {} sh -c "hyprctl clients -j  | jq -r '.[] | select(.title | test(\"{}\"))|.'"

}
