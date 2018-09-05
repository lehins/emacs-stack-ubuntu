#!/usr/bin/env bash

set -xe

# Setup latest version of stack
which stack && stack upgrade || {
  WGET_OR_CURL="$(which wget) -qO-" || unset WGET_OR_CURL
  WGET_OR_CURL="${WGET_OR_CURL:-$(which curl) -sSL}"
  $WGET_OR_CURL https://get.haskellstack.org/ | sh
# This seems to already get added by above script
#   cat >> ~/.profile <<EOF
# # required for stack
# if [ -d "$HOME/.local/bin" ] ; then
#     PATH='$HOME/.local/bin:$PATH'
# fi
# EOF
}

PATH="$HOME/.local/bin:$PATH" ./install-emacs.hs
