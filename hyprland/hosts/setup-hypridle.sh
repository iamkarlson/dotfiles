#!/bin/bash
# Setup host-specific hypridle configuration

HOSTNAME=$(hostnamectl hostname)
CONFIG_DIR="$HOME/.config/hypr"
HOSTS_DIR="$CONFIG_DIR/hosts"
TARGET_CONFIG="$CONFIG_DIR/hypridle.conf"

# Determine source config based on hostname
case "$HOSTNAME" in
    garnet)
        SOURCE_CONFIG="$HOSTS_DIR/garnet-hypridle.conf"
        ;;
    thinkpad|*)  # Default to thinkpad config for unknown hosts
        SOURCE_CONFIG="$HOSTS_DIR/thinkpad-hypridle.conf"
        ;;
esac

# Copy host-specific config to target location
if [ -f "$SOURCE_CONFIG" ]; then
    cp "$SOURCE_CONFIG" "$TARGET_CONFIG"
    echo "Hypridle config set for $HOSTNAME"
else
    echo "ERROR: Config not found: $SOURCE_CONFIG"
    exit 1
fi
