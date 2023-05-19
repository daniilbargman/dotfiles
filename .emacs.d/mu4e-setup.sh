#!/usr/bin/env bash
# vim: set filetype=sh :
#
# setup scripts for mu and mu4e
#
# Author: Daniil Bargman (daniil.bargman@gmail.com)
#
# base directory of this file accounting for symlinks
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until file is no longer a symlink
    BASE_DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
    SOURCE="$(readlink "$SOURCE")"
done
BASE_DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
[ -z "${SCRIPT_NAME:-}" ] && export SCRIPT_NAME="${0##*/}"
###############################################################################

# error-safe script settings
set -Eeuo pipefail

# default message appendix on errors
function append_error {
    bl=${BASH_LINENO[$((${#BASH_LINENO[@]} - 2))]}
    echo line $bl in $BASE_DIR/$SCRIPT_NAME
    exit 1
}

# trap errors from third-party functions
trap append_error ERR

# manual error handler: refer to help flag, and exit (use in place of echo)
function error_exit {
    echo
    echo $1
    append_error
}

###############################################################################

### MU4E ITSELF ###

## VERSION TO USE
VERSION=1.8

## install dependencies

sudo apt-get -y install libgmime-3.0-dev libxapian-dev meson

# install autotools for git repo to work
sudo apt-get -y install autotools-dev autoconf libtool texinfo

# optional
sudo apt-get -y install guile-2.2-dev html2text xdg-utils

# # optional: only needed for msg2pdf and mug (toy gtk+ frontend)
# sudo apt-get install libwebkitgtk-3.0-dev

# # install getmail
# sudo apt-get -y install getmail

# install mbsync with pass for managing mailboxes and credentials
sudo apt-get install -y isync pass

## install package itself

# get from git (alternatively, use a github tarball)
cd ~/.git-clones
git clone https://github.com/djcb/mu.git --branch release/${VERSION}
cd mu
# ./autogen.sh && make
# sudo make install
meson build && ninja -C build

###############################################################################

### OAUTH SUPPORT (FOR OUTLOOK) ###

# see https://sites.uw.edu/bxf4/2022/09/01/getting-uw-outlook-365-oauth2-to-work-with-emacs-mu4e-mbsync-and-msmtp/
 
# dependencies
sudo apt-get install -y \
     python3-pyxdg python3-msal python3-gnupg libsasl2-modules-kdexoauth2

# install oauth2ms and put on %PATH
cd ~/.git-clones
git clone https://github.com/harishkrupo/oauth2ms
cd oauth2ms
pip install -r requirements.txt
sudo ln -s $HOME/.git-clones/oauth2ms/oauth2ms /usr/local/bin/

# link wrapper script to path as well
sudo ln -s $HOME/.emacs.d/oauth2ms_wrapper /usr/local/bin/

# done.
