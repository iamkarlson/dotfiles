#!/usr/bin/env bash

# Test higher quality settings for 1920x1080
OUTPUT_DIR="$HOME/pictures/webcam/format_tests"
mkdir -p "$OUTPUT_DIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
DEVICE="/dev/video0"

echo "Testing higher quality settings for 1920x1080..."

# Test higher quality methods
declare -a methods=(
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=1920,height=1080 ! videoconvert ! jpegenc quality=100 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_1920x1080_q100.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=1920,height=1080 ! videoconvert ! jpegenc quality=98 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_1920x1080_q98.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=1920,height=1080 ! videoconvert ! pngenc ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_1920x1080_png.png\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=2304,height=1296 ! videoconvert ! jpegenc quality=95 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_2304x1296_q95.jpg\""
    "v4l2src device=\"$DEVICE\" num-buffers=1 ! video/x-raw,width=2304,height=1536 ! videoconvert ! jpegenc quality=95 ! filesink location=\"${OUTPUT_DIR}/${TIMESTAMP}_2304x1536_q95.jpg\""
)

declare -a names=(
    "1920x1080 quality=100 (max JPEG)"
    "1920x1080 quality=98"
    "1920x1080 PNG (lossless)"
    "2304x1296 quality=95 (higher res)"
    "2304x1536 quality=95 (max res)"
)

# Test each method
for i in "${!methods[@]}"; do
    method="${methods[$i]}"
    name="${names[$i]}"

    echo ""
    echo "Testing: $name"

    start_time=$(date +%s.%N)
    if timeout 15 gst-launch-1.0 $method 2>/dev/null; then
        end_time=$(date +%s.%N)
        duration=$(echo "$end_time - $start_time" | bc -l)

        # Find the output file
        output_file=$(echo "$method" | grep -o 'location="[^"]*"' | sed 's/location="//;s/"//')

        if [ -f "$output_file" ] && [ -s "$output_file" ]; then
            file_size=$(stat -c%s "$output_file")
            file_size_kb=$((file_size / 1024))
            echo "✅ Success! Size: ${file_size_kb}KB, Time: ${duration}s"
        else
            echo "❌ Failed - no output file or empty file"
        fi
    else
        echo "❌ Failed - command timeout or error"
    fi
done

echo ""
echo "High quality test completed! Compare with your preferred q95 image."
echo "File listing:"
ls -la "${OUTPUT_DIR}/${TIMESTAMP}_"*