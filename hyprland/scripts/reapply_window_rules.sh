#!/usr/bin/env bash
# Reapply workspace rules to all existing windows
# Reads patterns from hyprwhenthen config (single source of truth)

CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/hyprwhenthen/config.toml"

if [[ ! -f "$CONFIG" ]]; then
    echo "Config not found: $CONFIG"
    exit 1
fi

# Extract class->workspace mappings from hyprwhenthen config
declare -A RULES
current_pattern=""

while IFS= read -r line; do
    # Parse "when" lines to extract class pattern
    if [[ "$line" =~ when[[:space:]]*=[[:space:]]*\"(.*)\" ]]; then
        current_pattern="${BASH_REMATCH[1]}"
    fi
    # Parse "then" lines to extract workspace
    if [[ "$line" =~ movetoworkspacesilent[[:space:]]+([^\"]+)\" ]]; then
        workspace="${BASH_REMATCH[1]}"
        # Extract class from pattern (third field in openwindow format: addr,ws,class,title)
        if [[ "$current_pattern" =~ ^\.\*,\.\*,(.+),\.\*$ ]]; then
            class_regex="${BASH_REMATCH[1]}"
            RULES["$class_regex"]="$workspace"
        fi
        current_pattern=""
    fi
done < "$CONFIG"

if [[ ${#RULES[@]} -eq 0 ]]; then
    echo "No rules parsed from config"
    exit 1
fi

# Get all windows and apply matching rules
batch_cmd=""
matched=0

while IFS= read -r window; do
    addr=$(echo "$window" | jq -r '.address')
    class=$(echo "$window" | jq -r '.class')

    for pattern in "${!RULES[@]}"; do
        if [[ "$class" =~ $pattern ]]; then
            [[ -n "$batch_cmd" ]] && batch_cmd+="; "
            batch_cmd+="dispatch movetoworkspace ${RULES[$pattern]},address:$addr"
            ((matched++))
            break
        fi
    done
done < <(hyprctl clients -j | jq -c '.[]')

if [[ -n "$batch_cmd" ]]; then
    hyprctl --batch "$batch_cmd"
    echo "Reapplied rules to $matched window(s)"
else
    echo "No windows matched any rules"
fi
