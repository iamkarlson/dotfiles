#!/usr/bin/env bash
hostname=$(cat /etc/hostname)

if [ $hostname = "garnet" ]; then
    echo "home"
elif [ $hostname = "beryl" ]; then
    echo "work"
elif [ $hostname = "granite" ]; then
    echo "laptop"
else
    echo "CANNOT DETECT PROFILE" 2>&1 >>/tmp/log
fi
