#!/usr/bin/env bash

# Webcam Photo to Telegram Script
# Takes photos with all available webcams and sends them to Telegram

# Try to source environment variables from taskfile.env if not already set
if [ -z "$SMARTHOME_TELEGRAM_BOT_TOKEN" ] || [ -z "$SMARTHOME_TELEGRAM_BOT_CHAT" ]; then
    ENV_FILE="$HOME/src/dotfiles/tasks/taskfile.env"
    if [ -f "$ENV_FILE" ]; then
        echo "Loading environment variables from $ENV_FILE"
        set -a  # Export all variables
        source "$ENV_FILE"
        set +a  # Stop exporting
    fi
fi

# Check if required environment variables are set
if [ -z "$SMARTHOME_TELEGRAM_BOT_TOKEN" ]; then
    echo "Error: SMARTHOME_TELEGRAM_BOT_TOKEN not set" >&2
    echo "Please set it in $HOME/src/dotfiles/tasks/taskfile.env or as environment variable" >&2
    exit 1
fi

if [ -z "$SMARTHOME_TELEGRAM_BOT_CHAT" ]; then
    echo "Error: SMARTHOME_TELEGRAM_BOT_CHAT not set" >&2
    echo "Please set it in $HOME/src/dotfiles/tasks/taskfile.env or as environment variable" >&2
    exit 1
fi

# Create directories if they don't exist
mkdir -p ~/pictures/webcam
mkdir -p ~/.local/share/webcam-logs

# Generate timestamp for filenames
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
LOG_FILE=~/.local/share/webcam-logs/${TIMESTAMP}.log

# Function to check if Hyprland is running and notifications are available
is_notification_available() {
    pgrep -x "hyprland" >/dev/null 2>&1 && command -v notify-send >/dev/null 2>&1
}

# Function to send notifications (only if Hyprland is running)
send_notification() {
    local title="$1"
    local message="$2"
    local urgency="${3:-normal}"

    if is_notification_available; then
        notify-send "$title" "$message" -u "$urgency"
    fi
}

# Function to log messages
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a "$LOG_FILE"
}

# Function to send photo to Telegram
send_to_telegram() {
    local photo_path="$1"
    local device_name="$2"

    log_message "Sending photo from $device_name to Telegram..."

    local response=$(curl -s -X POST \
        "https://api.telegram.org/bot${SMARTHOME_TELEGRAM_BOT_TOKEN}/sendPhoto" \
        -F "chat_id=${SMARTHOME_TELEGRAM_BOT_CHAT}" \
        -F "photo=@${photo_path}" \
        -F "caption=📸 Photo from ${device_name} - $(date '+%Y-%m-%d %H:%M:%S')")

    # Check if the request was successful
    if echo "$response" | grep -q '"ok":true'; then
        log_message "✅ Successfully sent photo from $device_name"
        return 0
    else
        local error_msg=$(echo "$response" | grep -o '"description":"[^"]*"' | sed 's/"description":"//;s/"//')
        log_message "❌ Failed to send photo from $device_name: $error_msg"
        return 1
    fi
}

# Function to test if a video device can capture
test_video_device() {
    local device="$1"
    local test_file=$(mktemp --suffix=.jpg)

    # Test if device can capture using GStreamer
    if timeout 3 gst-launch-1.0 v4l2src device="$device" num-buffers=1 ! videoconvert ! jpegenc ! filesink location="$test_file" >/dev/null 2>&1; then
        if [ -f "$test_file" ] && [ -s "$test_file" ]; then
            rm -f "$test_file"
            return 0
        fi
    fi

    rm -f "$test_file"
    return 1
}

# Function to capture photo from webcam
capture_photo() {
    local device="$1"
    local device_name=$(basename "$device")
    local photo_path="$HOME/pictures/webcam/${TIMESTAMP}_${device_name}.jpg"

    log_message "Capturing photo from $device ($device_name)..."

    # Test the device first
    if ! test_video_device "$device"; then
        log_message "❌ Device $device_name failed capability test"
        return 1
    fi

    # Create a temporary error log to capture GStreamer output
    local error_log=$(mktemp)

    # Capture photo with maximum quality: 2304x1536 at 95% JPEG quality
    if timeout 15 gst-launch-1.0 v4l2src device="$device" num-buffers=1 ! video/x-raw,width=2304,height=1536 ! videoconvert ! jpegenc quality=95 ! filesink location="$photo_path" 2>"$error_log"; then
        if [ -f "$photo_path" ] && [ -s "$photo_path" ]; then
            local file_size=$(stat -c%s "$photo_path")
            local file_size_kb=$((file_size / 1024))
            log_message "✅ Successfully captured photo from $device_name (2304x1536, ${file_size_kb}KB)"
            rm -f "$error_log"

            # Send to Telegram
            if send_to_telegram "$photo_path" "$device_name"; then
                # Clean up photo file after successful send (optional)
                # rm "$photo_path"
                return 0
            else
                return 1
            fi
        else
            log_message "❌ Photo file is empty or doesn't exist for $device_name"
            cat "$error_log" >> "$LOG_FILE"
            rm -f "$error_log"
            return 1
        fi
    else
        log_message "❌ Failed to capture photo from $device ($device_name)"
        log_message "GStreamer error output:"
        cat "$error_log" >> "$LOG_FILE"
        rm -f "$error_log"
        return 1
    fi
}

# Main execution
log_message "Starting webcam photo capture and Telegram send process"

# Find all available video devices and filter functional ones
all_video_devices=($(ls /dev/video* 2>/dev/null))
video_devices=()

if [ ${#all_video_devices[@]} -eq 0 ]; then
    log_message "❌ No video devices found"
    send_notification "Webcam to Telegram" "No webcams found" "normal"
    exit 1
fi

log_message "Found ${#all_video_devices[@]} video device(s), testing capabilities..."

# Filter devices that can actually capture
for device in "${all_video_devices[@]}"; do
    if test_video_device "$device"; then
        video_devices+=("$device")
        log_message "✅ Device $(basename "$device") is functional"
    else
        log_message "⚠️ Device $(basename "$device") failed capability test, skipping"
    fi
done

if [ ${#video_devices[@]} -eq 0 ]; then
    log_message "❌ No functional video devices found"
    send_notification "Webcam to Telegram" "No functional webcams found" "normal"
    exit 1
fi

log_message "Using ${#video_devices[@]} functional device(s): ${video_devices[*]}"

# Counters for success/failure
success_count=0
total_count=${#video_devices[@]}

# Process each video device
for device in "${video_devices[@]}"; do
    if capture_photo "$device"; then
        ((success_count++))
    fi

    # Small delay between captures to avoid overwhelming the system
    sleep 1
done

# Final notification
if [ $success_count -eq $total_count ]; then
    send_notification "Webcam to Telegram" "✅ Successfully sent $success_count photo(s) to Telegram" "normal"
    log_message "🎉 Process completed successfully: $success_count/$total_count photos sent"
elif [ $success_count -gt 0 ]; then
    send_notification "Webcam to Telegram" "⚠️ Partially successful: $success_count/$total_count photos sent" "normal"
    log_message "⚠️ Process partially completed: $success_count/$total_count photos sent"
else
    send_notification "Webcam to Telegram" "❌ Failed to send any photos" "critical"
    log_message "❌ Process failed: 0/$total_count photos sent"
    exit 1
fi

log_message "Script execution finished"