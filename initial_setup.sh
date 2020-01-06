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

# update repositories
apt-get update

# NOTE: need to install Termite from source - may be distro-dependent

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

# install vim from source
apt-get install libncurses5-dev libgnome2-dev libgnomeui-dev \
    libgtk2.0-dev libatk1.0-dev libbonoboui2-dev \
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
apt-get install fonts-powerline

# vim plugin manager: vim-plug
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# tmux dependencies
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm  # plugin manager
apt-get install acpi  # for showing battery status
apt-get install sysstat  # for showing CPU percentages

# install herbstluftwm and dependencies
apt-get install bash-completion
apt-get install dzen2  # window/panel render
apt-get install conky  # system info display
apt-get install feh    # set background
apt-get install herbstluftwm
apt-get install compton  # compositor for setting transparency

#
