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

# # git
# sudo apt-get install -y git
# 
# # tmux (and xclip to copy from tmux to clipboard)
# sudo apt-get install -y tmux xclip
# 
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
git clone https://github.com/vim/vim.git && cd vim
./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp=yes \
            --enable-python3interp=yes \
            --with-python3-config-dir=$(python3-config --configdir) \
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
sudo apt-get -y install fonts-powerline fonts-firacode

# # vim plugin manager: vim-plug
# curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
#     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# # tmux dependencies
# git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm  # plugin manager

# install herbstluftwm and dependencies
sudo apt-get -y install acpi  # for showing battery status
sudo apt-get -y install sysstat  # for showing CPU percentages
sudo apt-get -y install bash-completion
sudo apt-get -y install dzen2  # window/panel render
sudo apt-get -y install conky  # system info display
sudo apt-get -y install feh    # set background
sudo apt-get -y install herbstluftwm
sudo apt-get -y install compton  # compositor for setting transparency
sudo apt-get -y install xdotool  # move mouse programmatically

### INSTALL SNAP ###
sudo apt-get -y install snap

### INSTALL HELM ###
sudo snap install --classic helm

### INSTALL NPM ###
sudo snap install --classic node

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
conda config --set changeps1 False

#
