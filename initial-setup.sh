#!/usr/bin/env bash
# vim: set filetype=sh
#
# Script for installing initial dependencies for a Debian-family Linux distro
#
# Author: Daniil Bargman (daniil.bargman@gmail.com)
#
# base directory of this script file
BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
###############################################################################

# Initial steps:
#   1. cd ~
#   2. git init
#   3. git add remote dotfiles https://github.com/daniilbargman/dotfiles
#   3. git pull dotfiles master
#
# NOTES:
#
#   1. HLWM autostart file assumes Emacs is installed using snap
#   2. Emacs configuration requires version 27+

set -Eeuo pipefail

# update repositories
sudo apt-get update && sudo apt-get -y upgrade

# install common dependencies
sudo apt-get install -y grub2 iwd curl keyutils s3cmd

# source .bashrc_ext from .bashrc
cat >> ~/.bashrc <<EOF

# Source my personalized settings
source ~/.bashrc_ext
EOF

# # optional: grub theme
# mkdir ~/.git-clones/
# cd ~/.git-clones/
# git clone --depth 1 https://gitlab.com/VandalByte/darkmatter-grub-theme.git
# cd darkmatter-grub-theme
# sudo python3 darkmatter-theme.py --install
# cd ~/.git-clones/

# build vim with the necessary dependencies

# sudo apt-get install -y libncurses5-dev libgtk2.0-dev libatk1.0-dev \
#     libcairo2-dev python-dev-is-python3
rm -rf vim && git clone https://github.com/vim/vim.git && cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-python3interp \
            --with-python3-command=/usr/bin/python3 \
            --with-python3-config-dir=$(/usr/bin/python3-config --configdir) \
            --enable-cscope \
            --prefix=/usr/local
make && sudo make install
make clean && make distclean

# # set vim as default editor
# sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 1
# sudo update-alternatives --set editor /usr/local/bin/vim
# sudo update-alternatives --install /usr/bin/vi vi /usr/local/bin/vim 1
# sudo update-alternatives --set vi /usr/local/bin/vim

# # vim plugin manager: vim-plug
# curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
#     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# # install fonts
# git clone --depth=1 https://github.com/ryanoasis/nerd-fonts
# sudo apt-get -y install fonts-firacode fonts-noto  # fonts-powerline 

# # bash completion
# sudo apt-get -y install bash-completion

# # optional: graphics manager: lightdm with autologin
# sudo apt-get -y install xorg
# cd /etc/X11 && sudo Xorg -configure
# cd ~
# sudo apt-get -y install lightdm

# # enable passwordless autologin
# cd ~/.config/lightdm
# # sudo addgroup autologin && sudo gpasswd -a $(logname) autologin
# # sed -i "s/autologin-user=.*/autologin-user=$(logname)/g" lightdm.conf 
# sudo cp {lightdm.conf,lightdm-gtk-greeter.conf} /etc/lightdm/
# sudo cp ~/.config/herbstluftwm/background.jpg /etc/lightdm/
# cd ~

# # install herbstluftwm and dependencies
# sudo apt-get -y install acpi  # for showing battery status
# sudo apt-get -y install sysstat  # for showing CPU percentages
# sudo apt-get -y install herbstluftwm

# # install compositor (compton)
# sudo apt-get -y install compton

# # install additional layout management programs
# sudo apt-get install -y polybar rofi feh lxappearance

# ### install alacritty terminal ###

# sudo apt-get -y install cmake pkg-config libfreetype6-dev libfontconfig1-dev \
# 	libxcb-xfixes0-dev libxkbcommon-dev python3
# curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# git clone https://github.com/alacritty/alacritty.git
# cd alacritty
# source ~/.cargo/env
# rustup override set stable
# rustup update stable
# cargo build --release
# sudo cp target/release/alacritty /usr/local/bin
# sudo cp extra/logo/alacritty-term.svg /usr/share/pixmaps/Alacritty.svg

# # bash completions
# echo "source $(pwd)/extra/completions/alacritty.bash" >> ~/.bashrc_ext

# ## INSTALL SNAP ###
# sudo apt-get -y install snapd

# ### INSTALL HELM ###
# sudo snap install --classic helm

### INSTALL NPM ###
sudo snap install --classic node
sudo ln -s "$(which npm)" /usr/local/bin/

# wait a little for it to take
sleep 10

# make global packages directory-local
mkdir "${HOME}/.npm-global"  # NOTE: this folder is added to PATH in .bashrc_ext
npm config set prefix "${HOME}/.npm-global"

### INSTALL MINICONDA ###

# NOTE: conda script needs to be sourced from bashrc by each user. A line for
# this is added to .bashrc_ext.

# Install our public GPG key to trusted store
curl https://repo.anaconda.com/pkgs/misc/gpgkeys/anaconda.asc | \
    gpg --dearmor > conda.gpg
sudo install -o root -g root -m 644 \
	conda.gpg /usr/share/keyrings/conda-archive-keyring.gpg

# Check whether fingerprint is correct (will output an error message otherwise)
gpg --keyring /usr/share/keyrings/conda-archive-keyring.gpg \
    --no-default-keyring --fingerprint \
    34161F5BF5EB1D4BFBBB8F0A8AEB4F8B29D82806

# Add our Debian repo
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/conda-archive-keyring.gpg] \
    https://repo.anaconda.com/pkgs/misc/debrepo/conda stable main" | \
    sudo tee -a /etc/apt/sources.list.d/conda.list

# install
sudo apt-get update
sudo apt-get -y install conda

# do not modify PS1 as this is handled in .bashrc_ext
source /opt/conda/etc/profile.d/conda.sh
conda config --set changeps1 False

# done
echo
echo "DONE!!!"
#
