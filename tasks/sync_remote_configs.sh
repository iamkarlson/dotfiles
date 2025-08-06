#!/usr/bin/env bash

# Remote config sync script
# Usage: sync_remote_configs.sh <hosts> <config_dirs> <temp_dir>

set -e

HOSTS="$1"
CONFIG_DIRS="$2"
TEMP_DIR="$3"

if [ -z "$HOSTS" ] || [ -z "$CONFIG_DIRS" ] || [ -z "$TEMP_DIR" ]; then
    echo "Usage: $0 <hosts> <config_dirs> <temp_dir>"
    echo "  hosts: comma-separated list of hostnames"
    echo "  config_dirs: comma-separated list of config directory names"
    echo "  temp_dir: temporary directory for sync operations"
    exit 1
fi

current_host=$(hostname)
IFS=',' read -ra host_list <<< "$HOSTS"

# Find target host (not current)
target_host=""
for host in "${host_list[@]}"; do
    if [ "$host" != "$current_host" ]; then
        target_host="$host"
        break
    fi
done

if [ -z "$target_host" ]; then
    echo "No target host found"
    exit 1
fi

echo "Syncing with $target_host"

# Create temp directory
mkdir -p "$TEMP_DIR"

IFS=',' read -ra config_dirs <<< "$CONFIG_DIRS"

for config_dir in "${config_dirs[@]}"; do
    local_path="$HOME/.config/$config_dir"
    remote_path="$HOME/.config/$config_dir"
    temp_remote="$TEMP_DIR/$config_dir"
    
    echo "=== $config_dir ==="
    
    # Download remote config
    mkdir -p "$temp_remote"
    if ssh "$target_host" "test -d $remote_path"; then
        rsync -az "$target_host:$remote_path/" "$temp_remote/"
    fi
    
    # Ensure local config exists
    mkdir -p "$local_path"
    
    # Find all project directories
    all_projects=($(find "$local_path" "$temp_remote" -maxdepth 1 -type d -printf '%f\n' 2>/dev/null | sort -u | grep -v '^$'))
    
    for project in "${all_projects[@]}"; do
        local_project="$local_path/$project"
        remote_project="$temp_remote/$project"
        
        if [ -d "$local_project" ] && [ -d "$remote_project" ]; then
            # Both exist - check for differences
            if ! diff -rq "$local_project" "$remote_project" >/dev/null 2>&1; then
                echo "⚠️  CONFLICT: $project exists on both machines with different content"
                
                while true; do
                    echo "Choose action: [l]ocal (this machine), [r]emote ($target_host), [d]iff, [s]kip"
                    read choice
                    case $choice in
                        l) 
                            echo "Using local version, updating remote..."
                            rsync -az "$local_project/" "$temp_remote/$project/" 
                            break
                            ;;
                        r) 
                            echo "Using remote version, updating local..."
                            rsync -az "$remote_project/" "$local_project/" 
                            break
                            ;;
                        d)
                            echo "=== INTERACTIVE DIFF: Local vs Remote ==="
                            echo "Press 'q' to exit diff view"
                            icdiff --line-numbers "$local_project" "$remote_project" 2>/dev/null 
                            ;;
                        s) 
                            echo "Skipped $project - no changes made" 
                            break
                            ;;
                        *) 
                            echo "Invalid option. Choose [l]ocal, [r]emote, [d]iff, or [s]kip"
                            ;;
                    esac
                done
            else
                echo "✅ $project: identical on both machines"
            fi
        elif [ -d "$local_project" ]; then
            # Local only - copy to remote
            echo "📤 COPYING: $project exists only locally, copying to $target_host"
            rsync -az "$local_project/" "$temp_remote/$project/"
        elif [ -d "$remote_project" ]; then
            # Remote only - copy to local
            echo "📥 COPYING: $project exists only on $target_host, copying to local"
            rsync -az "$remote_project/" "$local_project/"
        fi
    done
    
    # Upload changes back to remote
    if [ -d "$temp_remote" ]; then
        rsync -az --delete "$temp_remote/" "$target_host:$remote_path/"
    fi
done
