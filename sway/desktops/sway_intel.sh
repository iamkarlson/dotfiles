#!/usr/bin/env sh


##################################################
# Intel graphics
##################################################

export WLR_DRM_NO_ATOMIC=1
export WLR_NO_HARDWARE_CURSORS=1
export LIBVA_DRIVER_NAME=iHD
export VDPAU_DRIVER=va_gl
#export NVD_BACKEND=egl
#export NVD_BACKEND=direct



##################################################
# Wayland stuff
##################################################
export XDG_CURRENT_DESKTOP=sway
export XDG_SESSION_DESKTOP=sway
export XDG_SESSION_TYPE=wayland
export _JAVA_AWT_WM_NONREPARENTING=1


# Support xdf-desktop-portal-wlr based solutions, for example flameshot
export SDL_VIDEODRIVER=wayland
export _JAVA_AWT_WM_NONREPARENTING=1
export EGL_PLATFORM=wayland

#
# QT settings
#
export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORM=wayland
export QT_WAYLAND_DISABLE_WINDOWDECORATION=1



#
# Firefox and WebRTC stuff
#
export MOZ_DISABLE_RDD_SANDBOX=1
export MOZ_ENABLE_WAYLAND=1
export MOZ_WEBRENDER=1
export MOZ_ACCELERATED=1
export RTC_USE_PIPEWIRE=true

# Sway command line
#exec sway -d 2> ~/sway.log
exec sway --unsupported-gpu
