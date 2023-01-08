cd ~/src
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

cd ~
yay -S zsh-antidote wlroots-nvidia thefuck sddm i3status bemenu swaybg xorg-xwayland

antidote bundle <~/.zsh/plugins.txt >~/.zsh_plugins.zsh
