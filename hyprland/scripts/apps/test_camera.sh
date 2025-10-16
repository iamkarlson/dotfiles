#!/usr/bin/env bash

# Test camera capture
mkdir -p ~/pictures/webcam

# Test with different approaches
echo "Testing camera access..."

# Method 1: Direct ffmpeg
echo "Method 1: Direct ffmpeg with mjpeg"
if timeout 5 ffmpeg -f v4l2 -input_format mjpeg -video_size 640x480 -i /dev/video0 -frames:v 1 -y ~/pictures/webcam/test_method1.jpg 2>/dev/null; then
    echo "✅ Method 1 worked"
    ls -la ~/pictures/webcam/test_method1.jpg
else
    echo "❌ Method 1 failed"
fi

# Method 2: ffmpeg with default format
echo "Method 2: ffmpeg with default format"
if timeout 5 ffmpeg -f v4l2 -i /dev/video0 -frames:v 1 -y ~/pictures/webcam/test_method2.jpg 2>/dev/null; then
    echo "✅ Method 2 worked"
    ls -la ~/pictures/webcam/test_method2.jpg
else
    echo "❌ Method 2 failed"
fi

# Method 3: Using fswebcam if available
if command -v fswebcam >/dev/null; then
    echo "Method 3: fswebcam"
    if timeout 5 fswebcam -d /dev/video0 --no-banner ~/pictures/webcam/test_method3.jpg 2>/dev/null; then
        echo "✅ Method 3 worked"
        ls -la ~/pictures/webcam/test_method3.jpg
    else
        echo "❌ Method 3 failed"
    fi
fi

# Method 4: Using GStreamer
echo "Method 4: GStreamer"
if timeout 5 gst-launch-1.0 v4l2src device=/dev/video0 num-buffers=1 ! jpegenc ! filesink location=~/pictures/webcam/test_method4.jpg 2>/dev/null; then
    echo "✅ Method 4 worked"
    ls -la ~/pictures/webcam/test_method4.jpg
else
    echo "❌ Method 4 failed"
fi

# Method 5: GStreamer with different format
echo "Method 5: GStreamer with format conversion"
if timeout 5 gst-launch-1.0 v4l2src device=/dev/video0 num-buffers=1 ! videoconvert ! jpegenc ! filesink location=~/pictures/webcam/test_method5.jpg 2>/dev/null; then
    echo "✅ Method 5 worked"
    ls -la ~/pictures/webcam/test_method5.jpg
else
    echo "❌ Method 5 failed"
fi

# Check permissions
echo "Permission check:"
ls -la /dev/video*
echo "Current user groups:"
groups