#!/bin/bash

set -u

log() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $*"
}

ACTION="${1:-sleep}"

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

if ! command -v hyprctl >/dev/null 2>&1; then
    log "hyprctl not found"
    exit 0
fi

ensure_runtime_environment

case "${ACTION}" in
    sleep)
        log "sleep hook triggered"

        if hyprctl dispatch exec "hyprlock --immediate"; then
            log "hyprlock dispatch requested"
        else
            log "failed to dispatch hyprlock"
        fi

        sleep 0.5

        if hyprctl dispatch dpms off eDP-1; then
            log "DPMS off dispatched"
            sleep 1
            echo "Display status from hyprctl:"
            hyprctl monitors
        else
            log "failed to toggle DPMS"
        fi
        ;;
    resume)
        log "resume hook triggered"
        echo "hyprctl monitors:"
        hyprctl monitors

        if hyprctl dispatch dpms on eDP-1; then
            log "DPMS on dispatched"
        else
            log "failed to enable DPMS"
        fi
        ;;
    *)
        log "unknown action '${ACTION}'"
        ;;
esac
