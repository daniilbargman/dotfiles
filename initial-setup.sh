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
#   3. git pull https://github.com/daniilbargman/dotfiles.git
#
# NOTES:
#
#   1. HLWM autostart file assumes Emacs is installed using snap
#   2. Emacs configuration requires version 27+

set -Eeuo pipefail

# update repositories
sudo apt-get update && sudo apt-get -y upgrade

# NOTE: need to install Termite from source - may be distro-dependent

 # git
 sudo apt-get install -y git
 
 # # tmux (and xclip to copy from tmux to clipboard)
 # sudo apt-get install -y tmux xclip
 
 # # create folders integrated with .vimrc and .bashrc
 # mkdir -p ~/executables  # for storing executable bash scripts
 # mkdir -p ~/.backups/{vim,git,tmux,bash}  # for backing up edited dotfiles
 # mkdir -p ~/.backups/bash/.bashrc_ext  # for backing up bashrc extensions
 # mkdir -p ~/.backups/git/{.gitignore,.gitignore_global}  # for backing up gitignore
 # mkdir -p ~/.backups/tmux/.tmux.conf  # for backing up tmux config
 # mkdir -p ~/.backups/vim/{ftplugin,.myplugins.vim,.vimrc}  # for backing up vim config

 # source .bashrc_ext from .bashrc
 cat >> ~/.bashrc <<EOF

# Source my personalized settings
source ~/.bashrc_ext
EOF

# build vim with the necessary dependencies
sudo apt-get install -y libncurses5-dev \
    libgtk2.0-dev libatk1.0-dev \
    libcairo2-dev libx11-dev libxpm-dev libxt-dev python-dev \
    python3-dev ruby-dev lua5.1 lua5.1-dev libperl-dev git
rm -rf vim && git clone https://github.com/vim/vim.git && cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-python3interp \
            --with-python3-command=/usr/bin/python3 \
            --with-python3-config-dir=$(/usr/bin/python3-config --configdir) \
            --enable-perlinterp=yes \
            --enable-luainterp=yes \
            --enable-cscope \
            --prefix=/usr/local
make && sudo make install
make clean && make distclean

# set vim as default editor
sudo update-alternatives --install /usr/bin/editor editor /usr/local/bin/vim 1
sudo update-alternatives --set editor /usr/local/bin/vim
sudo update-alternatives --install /usr/bin/vi vi /usr/local/bin/vim 1
sudo update-alternatives --set vi /usr/local/bin/vim

# install fonts
git clone --depth=1 https://github.com/ryanoasis/nerd-fonts
sudo apt-get -y install fonts-powerline fonts-firacode fonts-noto

# bash completion
sudo apt-get -y install bash-completion

# vim plugin manager: vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# # tmux dependencies
# git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm  # plugin manager

# optional: graphics manager: lightdm with autologin
sudo apt-get -y install xorg
cd /etc/X11 && sudo Xorg -configure
cd ~
sudo apt-get -y install lightdm

# enable passwordless autologin
cd ~/.config/lightdm
sudo addgroup autologin && sudo gpasswd -a $(logname) autologin
sed -i "s/autologin-user=.*/autologin-user=$(logname)/g" lightdm.conf 
sudo cp {lightdm.conf,lightdm-gtk-greeter.conf} /etc/lightdm/
sudo cp ~/.config/herbstluftwm/background.jpg /etc/lightdm/
cd ~

# optional: grub theme
mkdir ~/.grub-themes
cd ~/.grub-themes
git clone https://github.com/vandalsoul/darkmatter-grub2-theme dark-matter
cd dark-matter
sudo python3 install.py Debian
cd ~

# install herbstluftwm and dependencies
sudo apt-get -y install acpi  # for showing battery status
sudo apt-get -y install sysstat  # for showing CPU percentages
sudo apt-get -y install herbstluftwm
# sudo apt-get -y install xdotool  # move mouse programmatically

# install compositor (compton)
sudo apt-get -y install compton

# # ALTERNATIVE: build compositor (picom)
# sudo apt-get install -y libxext-dev libxcb1-dev libxcb-damage0-dev \
#      libxcb-dpms0-dev libxcb-xfixes0-dev libxcb-shape0-dev \
#      libxcb-render-util0-dev libxcb-render0-dev libxcb-randr0-dev \
#      libxcb-composite0-dev libxcb-image0-dev libxcb-present-dev \
#      libxcb-xinerama0-dev libxcb-glx0-dev libpixman-1-dev libdbus-1-dev \
#      libconfig-dev libgl-dev libegl-dev libpcre2-dev libevdev-dev uthash-dev \
#      libev-dev libx11-xcb-dev meson asciidoc

# git clone https://github.com/yshui/picom picom && cd picom
# git submodule update --init --recursive
# meson setup --buildtype=release . build
# sudo ninja -C build install

# install additional layout management programs
sudo apt-get install -y polybar rofi feh lxappearance

# # i3-gaps
# sudo apt install -y meson dh-autoreconf libxcb-keysyms1-dev libpango1.0-dev \
#      libxcb-util0-dev xcb libxcb1-dev libxcb-icccm4-dev libyajl-dev libev-dev \
#      libxcb-xkb-dev libxcb-cursor-dev libxkbcommon-dev libxcb-xinerama0-dev \
#      libxkbcommon-x11-dev libstartup-notification0-dev libxcb-randr0-dev \
#      libxcb-xrm0 libxcb-xrm-dev libxcb-shape0 libxcb-shape0-dev
# git clone https://github.com/Airblader/i3 i3-gaps
# cd i3-gaps
# mkdir -p build && cd build
# meson --prefix /usr/local
# ninja
# sudo ninja install
# cd ~


### install alacritty terminal ###

sudo apt-get -y install cmake pkg-config libfreetype6-dev libfontconfig1-dev \
	libxcb-xfixes0-dev libxkbcommon-dev python3
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
git clone https://github.com/alacritty/alacritty.git
cd alacritty
source ~/.cargo/env
rustup override set stable
rustup update stable
cargo build --release
sudo cp target/release/alacritty /usr/local/bin
sudo cp extra/logo/alacritty-term.svg /usr/share/pixmaps/Alacritty.svg
sudo desktop-file-install extra/linux/Alacritty.desktop
sudo update-desktop-database

# bash completions
echo "source $(pwd)/extra/completions/alacritty.bash" >> ~/.bashrc_ext

# alacritty manpage
sudo mkdir -p /usr/local/share/man/man1
gzip -c extra/alacritty.man | \
    sudo tee /usr/local/share/man/man1/alacritty.1.gz > /dev/null
gzip -c extra/alacritty-msg.man | \
    sudo tee /usr/local/share/man/man1/alacritty-msg.1.gz > /dev/null

## INSTALL SNAP ###
sudo apt-get -y install snapd

### INSTALL HELM ###
sudo snap install --classic helm

### INSTALL NPM ###
sudo snap install --classic node

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

#
