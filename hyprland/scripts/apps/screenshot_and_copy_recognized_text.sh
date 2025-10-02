#!/usr/bin/env sh

# OCR Screenshot Script
# Takes a screenshot of selected area, performs OCR, and copies text to clipboard

# Create directories if they don't exist
mkdir -p ~/pictures/screenshots
mkdir -p ~/.local/share/ocr-logs

# Generate filenames with timestamp
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
IMG_FILE=~/pictures/screenshots/${TIMESTAMP}.png
OCR_LOG=~/.local/share/ocr-logs/${TIMESTAMP}.txt

# Take screenshot of selected area
grim -g "$(slurp)" "$IMG_FILE"

# Check if screenshot was taken successfully
if [ ! -f "$IMG_FILE" ]; then
    notify-send "Screenshot OCR" "Failed to take screenshot" -u critical
    exit 1
fi

# Perform OCR on the screenshot
OCR_TEXT=$(tesseract "$IMG_FILE" stdout 2>/dev/null)

# Check if OCR produced any text
if [ -z "$OCR_TEXT" ]; then
    ACTION=$(notify-send "Screenshot OCR" "No text recognized in image" -i "$IMG_FILE" -A "view=View Screenshot" -w)
    if [ "$ACTION" = "view" ]; then
        xdg-open "$IMG_FILE" &
    fi
    exit 0
fi

# Copy recognized text to clipboard
echo "$OCR_TEXT" | wl-copy

# Save OCR result to log file with metadata
{
    echo "Timestamp: $(date)"
    echo "Screenshot: $IMG_FILE"
    echo "Recognized text:"
    echo "$OCR_TEXT"
    echo "---"
} >> "$OCR_LOG"

# Send notification with OCR result and handle action
ACTION=$(notify-send "Screenshot OCR" "Text copied to clipboard: $(echo "$OCR_TEXT" | head -c 100)..." -i "$IMG_FILE" -A "view=View Screenshot" -w)
if [ "$ACTION" = "view" ]; then
    xdg-open "$IMG_FILE" &
fi
