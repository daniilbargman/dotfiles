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

# git
apt-get install git

# create folders integrated with .vimrc and .bashrc
mkdir ~/executables  # for storing executable bash scripts
mkdir -p ~/.backups/{vim,git,tmux,bash}  # for backing up edited dotfiles
mkdir ~/.backups/bash/.bashrc_ext  # for backing up bashrc extensions
mkdir ~/.backups/git/{.gitignore,.gitignore_global}  # for backing up gitignore
mkdir ~/.backups/tmux/.tmux.conf  # for backing up tmux config
mkdir ~/.backups/vim/{ftplugin,.myplugins.vim,.vimrc}  # for backing up vim config

# clone my dotfiles
git clone https://github.com/dbargman/dotfiles ~

# source .bashrc_ext from .bashrc
cat >> ~/.bashrc <<EOF

# Source my personalized settings
source ~/.bashrc_ext
EOF

# powerline fonts - mainly for tmux status line
apt-get install fonts-powerline

# tmux dependencies
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm  # plugin manager
apt-get install acpi  # for showing battery status
apt-get install sysstat  # for showing CPU percentages

# install herbstluftwm and dependencies
apt-get install bash-completion
apt-get install dzen2
apt-get install feh
apt-get install herbstluftwm

#
