#!/usr/bin/env bash
# vim: set filetype=sh
#
# Build GCC Emacs from source
#
# Author: Daniil Bargman (daniil.bargman@gmail.com)
#
# base directory of this script file
BASE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
###############################################################################

set -Eeuo pipefail

# update repositories
sudo apt-get update && sudo apt-get -y upgrade

# install main Emacs dependencies for Debian
# (see https://www.emacswiki.org/emacs/BuildingEmacs)

# additional dependencies are for GCCEmacs and vterm-mode
sudo apt-get install -y libc6-dev libjpeg62-turbo libpng-dev \
     libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev \
     libgccjit-14-dev cmake libtool libtool-bin librsvg2-dev \
     build-essential libgtk-3-dev libgnutls28-dev libjpeg-dev libxpm-dev \
     libncurses-dev autoconf make texinfo texlive texlive-latex-extra \
     texlive-bibtex-extra texlive-lang-engligh latexmk gnutls-bin gcc \
     libxpm-dev libmagickcore-dev libmagick++-dev libgif-dev fonts-firacode \
     mailutils libsqlite3-dev

# install pinentry-gnome3 to get a decent-looking passphrase popup in emacs
# when updating something that requires access to the pass store
sudo apt-get install -y pinentry-gnome3

# NOTE: libjansson dependency and --with-json flag are obsolete as of Emacs 30
# # install libjansson for faster LSP mode
# mkdir -p ~/.git-clones
# cd ~/.git-clones
# rm -rf jansson
# git clone https://github.com/akheron/jansson
# cd jansson
# autoreconf -i
# ./configure
# make
# sudo make install

# tree-sitter
cd ~/.git-clones
git clone https://github.com/tree-sitter/tree-sitter.git
cd tree-sitter/
make
sudo make install
sudo ldconfig

# emacs itself
cd ~/.git-clones
sudo rm -rf emacs
git clone --depth 1 https://git.savannah.gnu.org/git/emacs.git

# configure with considerations from StackOverflow
# note: xwidgets is broken in latest emacs - see
# https://www.reddit.com/r/emacs/comments/1fpd3dk/problem_compiling_latest_git_version/
cd ~/.git-clones/emacs
./autogen.sh
./configure --with-cairo --with-mailutils --with-tree-sitter \
    --with-imagemagick --with-x-toolkit=gtk3 --with-native-compilation # --with-xwidgets  

# make and make install
sudo make -j$(nproc)
sudo make install

#
