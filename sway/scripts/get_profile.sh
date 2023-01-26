#!/usr/bin/env bash
hostname=$(cat /etc/hostname)

echo $hostname >> /tmp/log

if [ $hostname = "garnet" ]; then
    echo "home" >&1
elif [ $hostname = "beryl" ]; then
    echo "work" >&1
else
    echo "CANNOT DETECT PROFILE">>/tmp/log
fi
