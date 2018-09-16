#!/usr/bin/env bash

set -xe

./install-stack.sh
PATH="$HOME/.local/bin:$PATH" ./install-emacs.hs
