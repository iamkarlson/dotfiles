#!/usr/bin/env sh

#Sourcing profile settings
export QT_QPA_PLATFORMTHEME=qt5ct


#Sway nvidia settings 
export WLR_NO_HARDWARE_CURSORS=1
export XDG_SESSION_TYPE=wayland
export QT_QPA_PLATFORM=wayland
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
export GBM_BACKEND=nvidia-drm
export __GL_SYNC_ALLOWED=0
export __GL_VRR_ALLOWED=0
export __GLX_VENDOR_LIBRARY_NAME=nvidia

exec sway --unsupported-gpu
