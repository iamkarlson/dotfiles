#!/usr/bin/env sh
#
# Unified screenshot tool for Hyprland
# Usage: screenshot.sh <mode> [--edit] [--ocr]
#
# Modes:
#   monitor  - capture focused monitor
#   area     - select area with slurp (freeze-capture to avoid overlay artifacts)
#   screen   - capture full screen
#
# Flags:
#   --edit   - open in swappy for annotation before copying
#   --ocr    - extract text with tesseract instead of copying the image

SCREENSHOT_DIR=~/pictures/screenshots
OCR_LOG_DIR=~/.local/share/ocr-logs
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
FILE_NAME="${TIMESTAMP}.png"
IMG_FILE="${SCREENSHOT_DIR}/${FILE_NAME}"

mkdir -p "$SCREENSHOT_DIR"

# Parse arguments
MODE="$1"
shift
EDIT=false
OCR=false
for arg in "$@"; do
    case "$arg" in
        --edit) EDIT=true ;;
        --ocr)  OCR=true ;;
    esac
done

# Capture
case "$MODE" in
    monitor)
        MONITOR=$(hyprctl -j monitors | jq -r '.[] | select(.focused == true) | .name')
        grim -o "$MONITOR" "$IMG_FILE"
        ;;
    area)
        # Freeze: capture full screen first, then let user select, then crop
        TEMP=$(mktemp /tmp/screenshot-XXXXXX.png)
        grim "$TEMP"
        GEOM=$(slurp)
        if [ -z "$GEOM" ]; then
            rm -f "$TEMP"
            exit 0
        fi
        CROP=$(echo "$GEOM" | sed 's/\([0-9]*\),\([0-9]*\) \(.*\)/\3+\1+\2/')
        magick "$TEMP" -crop "$CROP" +repage "$IMG_FILE"
        rm -f "$TEMP"
        ;;
    screen)
        grim "$IMG_FILE"
        ;;
    *)
        echo "Usage: screenshot.sh <monitor|area|screen> [--edit] [--ocr]" >&2
        exit 1
        ;;
esac

if [ ! -f "$IMG_FILE" ]; then
    notify-send "Screenshot" "Failed to capture screenshot" -u critical
    exit 1
fi

# Post-process
if [ "$OCR" = true ]; then
    mkdir -p "$OCR_LOG_DIR"
    OCR_LOG="${OCR_LOG_DIR}/${TIMESTAMP}.txt"
    OCR_TEXT=$(tesseract "$IMG_FILE" stdout 2>/dev/null)

    if [ -z "$OCR_TEXT" ]; then
        ACTION=$(notify-send "Screenshot OCR" "No text recognized in image" \
            -i "$IMG_FILE" -A "view=View Screenshot" -w)
        [ "$ACTION" = "view" ] && xdg-open "$IMG_FILE" &
        exit 0
    fi

    echo "$OCR_TEXT" | wl-copy
    {
        echo "Timestamp: $(date)"
        echo "Screenshot: $IMG_FILE"
        echo "Recognized text:"
        echo "$OCR_TEXT"
        echo "---"
    } >> "$OCR_LOG"

    ACTION=$(notify-send "Screenshot OCR" \
        "Text copied to clipboard: $(echo "$OCR_TEXT" | head -c 100)..." \
        -i "$IMG_FILE" -A "view=View Screenshot" -w)
    [ "$ACTION" = "view" ] && xdg-open "$IMG_FILE" &
    exit 0
fi

if [ "$EDIT" = true ]; then
    swappy -f "$IMG_FILE" -o "$IMG_FILE"
fi

ACTION=$(notify-send "Screenshot" "$FILE_NAME" \
    -i "$IMG_FILE" \
    -A "default=Open" \
    -A "copy=Copy to clipboard" \
    -w)

case "$ACTION" in
    default) xdg-open "$IMG_FILE" & ;;
    copy)    wl-copy < "$IMG_FILE" ;;
esac
