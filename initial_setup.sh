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

# Note: initial steps:
#   1. cd ~
#   2. git init
#   3. git pull https://github.com/daniilbargman/dotfiles.git

set -Eeuo pipefail

# update repositories
sudo apt-get update && sudo apt-get -y upgrade

# NOTE: need to install Termite from source - may be distro-dependent

# git
sudo apt-get install -y git

# tmux (and xclip to copy from tmux to clipboard)
sudo apt-get install -y tmux xclip

# create folders integrated with .vimrc and .bashrc
mkdir -p ~/executables  # for storing executable bash scripts
mkdir -p ~/.backups/{vim,git,tmux,bash}  # for backing up edited dotfiles
mkdir -p ~/.backups/bash/.bashrc_ext  # for backing up bashrc extensions
mkdir -p ~/.backups/git/{.gitignore,.gitignore_global}  # for backing up gitignore
mkdir -p ~/.backups/tmux/.tmux.conf  # for backing up tmux config
mkdir -p ~/.backups/vim/{ftplugin,.myplugins.vim,.vimrc}  # for backing up vim config

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

 # powerline fonts - mainly for tmux status line
 sudo apt-get install fonts-powerline

 # vim plugin manager: vim-plug
 curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

 # tmux dependencies
 git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm  # plugin manager
 sudo apt-get install acpi  # for showing battery status
 sudo apt-get install sysstat  # for showing CPU percentages

 # install herbstluftwm and dependencies
 sudo apt-get install bash-completion
 sudo apt-get install dzen2  # window/panel render
 sudo apt-get install conky  # system info display
 sudo apt-get install feh    # set background
 sudo apt-get install herbstluftwm
 sudo apt-get install compton  # compositor for setting transparency

#
