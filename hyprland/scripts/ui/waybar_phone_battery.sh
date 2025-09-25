#!/bin/bash

# KDE Connect phone battery script for waybar
DEVICE_ID="755744aa_dac6_43bc_8fd5_16676e4eb440"
OBJECT_PATH="/modules/kdeconnect/devices/$DEVICE_ID/battery"
INTERFACE="org.kde.kdeconnect.device.battery"

# Check if device is connected by trying to introspect
if ! gdbus introspect --session --dest org.kde.kdeconnect --object-path "$OBJECT_PATH" >/dev/null 2>&1; then
	# Phone disconnected - show disconnected status
	echo "📱 X"
	exit 0
fi

# Get battery charge and charging status
CHARGE=$(gdbus call --session --dest org.kde.kdeconnect --object-path "$OBJECT_PATH" --method org.freedesktop.DBus.Properties.Get "$INTERFACE" "charge" 2>/dev/null | sed 's/[<>()variant]//g' | tr -d ' ,' | sed 's/^int32//')
IS_CHARGING=$(gdbus call --session --dest org.kde.kdeconnect --object-path "$OBJECT_PATH" --method org.freedesktop.DBus.Properties.Get "$INTERFACE" "isCharging" 2>/dev/null | sed 's/[<>(),]//g' | tr -d ' ')

# If we couldn't get the data, show disconnected
if [[ -z "$CHARGE" ]]; then
	echo "📱 X"
	exit 0
fi

ICON="📱"

# Choose icon based on charging status and battery level
if [[ "$IS_CHARGING" == "true" ]]; then
	CHARGE_STATE_ICON="🔌"
else
	# Choose battery icon based on charge level
	if [[ $CHARGE -gt 90 ]]; then
		CHARGE_STATE_ICON=""
	elif [[ $CHARGE -gt 70 ]]; then
		CHARGE_STATE_ICON=""

	elif [[ $CHARGE -gt 50 ]]; then
		CHARGE_STATE_ICON=""
	elif [[ $CHARGE -gt 30 ]]; then
		CHARGE_STATE_ICON=""
	else
		CHARGE_STATE_ICON=""
	fi
fi

# Output format: icon + percentage
echo "[${ICON}${CHARGE_STATE_ICON} ${CHARGE}%]"
