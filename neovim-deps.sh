#!/bin/bash
pyenv install 2.7.11
pyenv install 3.7.0

pyenv virtualenv 2.7.11 neovim2
pyenv virtualenv 3.7.0 neovim3

pyenv activate neovim2
pip install neovim
pyenv which python  # Note the path

pyenv activate neovim3
pip install neovim
pyenv which python  # Note the path

# The following is optional, and the neovim3 env is still active
# This allows flake8 to be available to linter plugins regardless
# of what env is currently active.  Repeat this pattern for other
# packages that provide cli programs that are used in Neovim.
# pip install flake8
# ln -s `pyenv which flake8` ~/bin/flake8  # Assumes that $HOME/bin is in $PATH
