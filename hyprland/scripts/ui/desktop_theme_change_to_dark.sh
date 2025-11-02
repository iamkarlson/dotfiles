#!/bin/bash
dbus-send --session --dest=org.freedesktop.portal.Desktop /org/freedesktop/portal/desktop org.freedesktop.impl.portal.Settings.SetOne string:"org.freedesktop.appearance" string:"color-scheme" variant:uint32:1

gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
