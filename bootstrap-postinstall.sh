cd ~/src
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

cd ~
yay -S \
    zsh-antidote \
    wlroots-nvidia \
    thefuck \
    sddm \
    rofi \
    swaybg \
    waybar \
    swaylock-effects-git \
    xorg-xwayland

antidote bundle <~/.zsh/plugins.txt >~/.zsh_plugins.zsh




wget https://framagit.org/MarianArlt/sddm-sugar-candy/-/archive/master/sddm-sugar-candy-master.tar.gz \
    -O sugar-candy.tar.gz

sudo tar -xzvf ~/Downloads/sugar-candy.tar.gz -C /usr/share/sddm/themes
