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

# install main Emacs dependencies for Debian buster
# (see https://www.emacswiki.org/emacs/BuildingEmacs)

# additional dependencies are for GCCEmacs and vterm-mode
sudo apt-get install -y libc6-dev libjpeg62-turbo libncurses5-dev libpng-dev \
     libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev \
     libgccjit-12-dev cmake libtool libtool-bin texinfo librsvg2-dev

# install fonts
sudo apt-get install -y fonts-powerline fonts-firacode

# install recommended dependencies based on StackOverflow answer
# (see https://superuser.com/questions/1128721/compiling-emacs-25-1-on-ubuntu-16-04/1129052#1129052)
sudo apt-get install -y libgtk-3-dev libwebkit2gtk-4.0-dev gnutls-dev

# install libjansson for faster LSP mode
mkdir -p ~/.git-clones
cd ~/.git-clones
rm -rf jansson
git clone https://github.com/akheron/jansson
cd jansson
autoreconf -i
./configure
make
sudo make install

# # # clone the repo and go there
cd ~/.git-clones
sudo rm -rf emacs
git clone --depth 1 https://git.savannah.gnu.org/git/emacs
cd emacs

# configure with considerations from StackOverflow
./autogen.sh
./configure --with-json --with-cairo --with-xwidgets \
	    --with-x-toolkit=gtk3 \
	    --with-native-compilation  # this is for the GCC branch of Emacs

# make and make install
sudo make -j$(nproc)
sudo make install

#
