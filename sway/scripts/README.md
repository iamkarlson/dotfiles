This folder contains bunch of helper scripts alongside of systemctl timer and other stuff to do stuff on the schedule. 

``` shell
cp *.timer ~/.config/systemd/user
cp *.service ~/.config/systemd/user

systemctl daemon-reload

systemctl --user enable wallpaper-cycle.timer
systemctl --user start wallpaper-cycle.timer
```
