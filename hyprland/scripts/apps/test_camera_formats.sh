#!/usr/bin/env bash

# Test different camera capture formats and qualities
OUTPUT_DIR="$HOME/pictures/webcam/format_tests"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
DEVICE="/dev/video0"

echo "Testing different capture formats for $DEVICE..."
echo "All test images will be saved to $OUTPUT_DIR/"

# Test methods array
declare -a methods=(
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=1920,height=1080 ! videoconvert ! jpegenc quality=95 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_1920x1080_q95.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=1280,height=720 ! videoconvert ! jpegenc quality=90 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_1280x720_q90.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=640,height=480 ! videoconvert ! jpegenc quality=95 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_640x480_q95.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! videoconvert ! jpegenc quality=95 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_default_q95.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! videoconvert ! jpegenc ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_default_basic.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! jpegenc ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_direct_jpeg.jpg\""
)

declare -a names=(
    "1920x1080 quality=95"
    "1280x720 quality=90"
    "640x480 quality=95"
    "default resolution quality=95"
    "default resolution basic"
    "direct JPEG (no conversion)"
)

# Test each method
for i in "${!methods[@]}"; do
    method="${methods[$i]}"
    name="${names[$i]}"

    echo ""
    echo "Testing: $name"
    echo "Command: gst-launch-1.0 $method"

    start_time=$(date +%s.%N)
    if timeout 10 gst-launch-1.0 $method 2>/dev/null; then
        end_time=$(date +%s.%N)
        duration=$(echo "$end_time - $start_time" | bc -l)

        # Find the output file
        output_file=$(echo "$method" | grep -o 'location="[^"]*"' | sed 's/location="//;s/"//')

        if [ -f "$output_file" ] && [ -s "$output_file" ]; then
            file_size=$(stat -c%s "$output_file")
            file_size_kb=$((file_size / 1024))
            echo "✅ Success! File: $(basename "$output_file"), Size: ${file_size_kb}KB, Time: ${duration}s"
        else
            echo "❌ Failed - no output file or empty file"
        fi
    else
        echo "❌ Failed - command timeout or error"
    fi
done

echo ""
echo "Test completed! Check the files in $OUTPUT_DIR/"
echo "File listing:"
ls -la "${OUTPUT_DIR}/${TIMESTAMP}_"*