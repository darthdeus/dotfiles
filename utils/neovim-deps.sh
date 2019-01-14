#!/bin/bash
set -euxo pipefail

VENVS_DIR="$HOME/.venvs"

mkdir -p "$VENVS_DIR"

virtualenv -p python2 "$VENVS_DIR/neovim2"
"$VENVS_DIR/neovim2/bin/pip" install neovim

virtualenv -p python3 "$VENVS_DIR/neovim3"
"$VENVS_DIR/neovim3/bin/pip" install neovim mypy flake8

# python2 -m pip install virtualenv
# python3 -m pip install virtualenv
#
# python2 -m virtualenv ~/.venv/neovim2
# ~/.venv/neovim2/bin/pip install neovim
#
# python3 -m virtualenv ~/.venv/neovim3
# ~/.venv/neovim3/bin/pip install neovim mypy flake8


# The following is optional, and the neovim3 env is still active
# This allows flake8 to be available to linter plugins regardless
# of what env is currently active.  Repeat this pattern for other
# packages that provide cli programs that are used in Neovim.
# pip install flake8
# ln -s `pyenv which flake8` ~/bin/flake8  # Assumes that $HOME/bin is in $PATH
