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
#
# additional dependencies are for GCCEmacs and vterm-mode
sudo apt-get install libc6-dev libjpeg62-turbo libncurses5-dev libpng-dev \
     libtiff5-dev libgif-dev xaw3dg-dev zlib1g-dev libx11-dev \
     libgccjit-8-dev \
     cmake libtool libtool-bin

# install recommended dependencies based on StackOverflow answer
# (see https://superuser.com/questions/1128721/compiling-emacs-25-1-on-ubuntu-16-04/1129052#1129052)
sudo apt-get install libgtk-3-dev libwebkit2gtk-4.0-dev gnutls-dev

# clone the repo and go there
 # git clone git://git.savannah.gnu.org/emacs.git -b feature/native-comp
 cd emacs

# configure with considerations from StackOverflow
./autogen.sh
./configure --with-cairo --with-xwidgets --with-x-toolkit=gtk3 \
	    --with-native-compilation  # this is for the GCC branch of Emacs

# make and make install
sudo make -j$(nproc)
make install

#
