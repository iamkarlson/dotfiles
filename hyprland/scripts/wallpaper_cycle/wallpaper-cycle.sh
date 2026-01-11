#!/usr/bin/env bash
set -e;

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $*"
}

ensure_runtime_environment() {
    if [ -z "${XDG_RUNTIME_DIR:-}" ]; then
        export XDG_RUNTIME_DIR="/run/user/$(id -u)"
        log "XDG_RUNTIME_DIR not set, defaulting to ${XDG_RUNTIME_DIR}"
    fi

    if [ -z "${HYPRLAND_INSTANCE_SIGNATURE:-}" ]; then
        local hypr_dir="${XDG_RUNTIME_DIR}/hypr"
        local signature=""

        if [ -d "${hypr_dir}" ]; then
            while IFS= read -r candidate; do
                [ -d "${candidate}" ] || continue
                if [ -S "${candidate}/.socket.sock" ]; then
                    signature=$(basename "${candidate}")
                    break
                fi
            done < <(find "${hypr_dir}" -mindepth 1 -maxdepth 1 -type d -print 2>/dev/null)
        fi

        if [ -n "${signature}" ]; then
            export HYPRLAND_INSTANCE_SIGNATURE="${signature}"
            log "HYPRLAND_INSTANCE_SIGNATURE inferred as ${HYPRLAND_INSTANCE_SIGNATURE}"
        else
            log "no Hyprland instance signature found in ${hypr_dir}"
            exit 0
        fi
    fi

    if [ -z "${WAYLAND_DISPLAY:-}" ]; then
        local wayland_socket
        wayland_socket=$(find "${XDG_RUNTIME_DIR}" -maxdepth 1 -type s -name 'wayland-*' -print -quit 2>/dev/null || true)
        if [ -n "${wayland_socket}" ]; then
            export WAYLAND_DISPLAY="$(basename "${wayland_socket}")"
            log "WAYLAND_DISPLAY inferred as ${WAYLAND_DISPLAY}"
        else
            log "WAYLAND_DISPLAY not set and no wayland sockets found"
        fi
    fi
}

ensure_runtime_environment

# Determining a directory with wallpapers
CONFIG_DIR="$HOME/.config/wallpaper_cycle"
CONFIG_FILE="$CONFIG_DIR/config.yaml"
DEFAULT_WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"

# Ensure config directory exists
mkdir -p "$CONFIG_DIR"

# Ensure config file exists
if [ ! -f "$CONFIG_FILE" ]; then
  echo "Creating default config.yaml at $CONFIG_FILE"
  cat <<EOF > "$CONFIG_FILE"
wallpapers_dir: "$DEFAULT_WALLPAPERS_DIR"
EOF
fi

# Read the wallpapers_dir from config.yaml or use default
WALLPAPERS_DIR=$(grep "^wallpapers_dir:" "$CONFIG_FILE" | sed -E 's/^wallpapers_dir:\s*"([^"]+)"$/\1/')
WALLPAPERS_DIR=${WALLPAPERS_DIR:-$DEFAULT_WALLPAPERS_DIR}

echo "Using wallpapers directory: $WALLPAPERS_DIR"


if [ -z "$WALLPAPERS_DIR" ]; then
  echo "WALLPAPERS_DIR is not set."
  echo "wallpapers dir does not exist - using fallback"
  WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"
elif [ -d $WALLPAPERS_DIR ]; then
  echo "wallpapers dir exists"

fi

#WALLPAPERS_DIR="$HOME/pictures/wallpapers/collection"

echo "wallpapers dir $WALLPAPERS_DIR"
echo "files count: "
ls -1 $WALLPAPERS_DIR | wc -l

# File to store the current wallpaper
CURRENT_WALLPAPER_FILE="$CONFIG_DIR/.current_wallpaper"

# Get list of monitors
MONITORS=$(hyprctl monitors -j | jq -r '.[].name')

# Function to get next wallpaper
get_next_wallpaper() {
  local last_wallpaper="$1"
  local wallpapers=("$WALLPAPERS_DIR"/*)

  if [ -z "$last_wallpaper" ]; then
    # Pick a random wallpaper if no last wallpaper is set
    echo "${wallpapers[RANDOM % ${#wallpapers[@]}]}"
  else
    # Find the index of the last wallpaper
    local index=-1
    for i in "${!wallpapers[@]}"; do
      if [ "${wallpapers[$i]}" == "$last_wallpaper" ]; then
        index=$i
        break
      fi
    done

    # Get the next wallpaper in the array
    echo "${wallpapers[((index + 1) % ${#wallpapers[@]})]}"
  fi
}

# Check if .current_wallpaper file exists
if [ -f "$CURRENT_WALLPAPER_FILE" ]; then
  LAST_WALLPAPER=$(cat "$CURRENT_WALLPAPER_FILE")
else
  LAST_WALLPAPER=""
fi

hyprctl hyprpaper listloaded | xargs -I {} hyprctl hyprpaper unload {}

# Iterate over each monitor and set wallpaper
for MONITOR in $MONITORS; do
  NEW_WALLPAPER=$(get_next_wallpaper "$LAST_WALLPAPER")
  echo "hyprctl hyprpaper preload $NEW_WALLPAPER"
  hyprctl hyprpaper preload $NEW_WALLPAPER -j --instance 0
  echo "hyprctl hyprpaper wallpaper $MONITOR,$NEW_WALLPAPER"
  hyprctl hyprpaper wallpaper $MONITOR,$NEW_WALLPAPER -j --instance 0

  echo "$NEW_WALLPAPER" > "$CURRENT_WALLPAPER_FILE"
  LAST_WALLPAPER="$NEW_WALLPAPER"
done


