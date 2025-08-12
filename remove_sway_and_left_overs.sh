#!/usr/bin/env bash
#
function delete_link(){
	[ -h "$1" ] && rm -i "$1"
}

# Removing file -------------------------------------- /home/iamkarlson/.config/sway/config
delete_link "/home/iamkarlson/.config/sway/config"
# Removing directory ********************************* /home/iamkarlson/.config/sway/config.d
delete_link "/home/iamkarlson/.config/sway/config.d"
# Removing directory ********************************* /home/iamkarlson/.config/sway/scripts
delete_link "/home/iamkarlson/.config/sway/scripts"
# Removing directory ********************************* /home/iamkarlson/.config/sway/profiles
delete_link "/home/iamkarlson/.config/sway/profiles"
# Removing file -------------------------------------- /home/iamkarlson/.config/swaylock/config
delete_link "/home/iamkarlson/.config/swaylock/config"
# Removing directory ********************************* /home/iamkarlson/.config/kanshi
delete_link "/home/iamkarlson/.config/kanshi"

# Removing file -------------------------------------- /usr/bin/sway_nvidia
delete_link "/usr/bin/sway_nvidia"
# Removing file -------------------------------------- /usr/bin/sway_amd
delete_link "/usr/bin/sway_amd"
# Removing file -------------------------------------- /usr/bin/sway_intel
delete_link "/usr/bin/sway_intel"
