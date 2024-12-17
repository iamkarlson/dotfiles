#!/bin/zsh

cp *.timer ~/.config/systemd/user
cp *.service ~/.config/systemd/user

systemctl daemon-reload

systemctl --user enable wallpaper-cycle.timer
systemctl --user start wallpaper-cycle.timer
