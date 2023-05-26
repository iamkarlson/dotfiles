#!/usr/bin/env sh

#
# QT settings
#
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORM=wayland
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1


#
# NVIDIA settings
#
#export WLR_DRM_NO_ATOMIC=1
#export WLR_NO_HARDWARE_CURSORS=1
#export GBM_BACKEND=nvidia-drm
#export __GL_SYNC_ALLOWED=0
#export __GL_VRR_ALLOWED=0
#export __GLX_VENDOR_LIBRARY_NAME=nvidia
#export __NV_PRIME_RENDER_OFFLOAD=1
#export __VK_LAYER_NV_optimus=NVIDIA_only
#export LIBVA_DRIVER_NAME=nvidia
#export VDPAU_DRIVER=nvidia
#export NVD_BACKEND=egl
#export NVD_BACKEND=direct
#export NVD_LOG=/tmp/nvd.log


#export WLR_DRM_DEVICES=/dev/dri/card1
#

# AMD

export LIBVA_DRIVER_NAME=radeonsi


#
# Intel graphics

#
# Wayland stuff
#
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_DESKTOP=sway
export XDG_SESSION_TYPE=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
#
# Support xdf-desktop-portal-wlr based solutions, for example flameshot
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
export EGL_PLATFORM=wayland



#
# Firefox and WebRTC stuff
#
export MOZ_DISABLE_RDD_SANDBOX=1
export MOZ_ENABLE_WAYLAND=1
export MOZ_WEBRENDER=1
export MOZ_ACCELERATED=1
export RTC_USE_PIPEWIRE=true

# Sway command line
exec sway -d 2> ~/sway.log
#exec sway --unsupported-gpu
