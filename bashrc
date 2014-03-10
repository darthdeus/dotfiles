# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export EDITOR="vim"

# If not running interactively, don't do anything
if [[ -n "$PS1" ]]; then

  . ~/bin/dotfiles/bash/env

  # check the window size after each command and, if necessary,
  # update the values of LINES and COLUMNS.
  shopt -s checkwinsize

  # make less more friendly for non-text input files, see lesspipe(1)
  [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

  # enable programmable completion features (you don't need to enable
  # this, if it's already enabled in /etc/bash.bashrc and /etc/profile
  # sources /etc/bash.bashrc).
  if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
      . /etc/bash_completion
  fi

  # . ~/bin/dotfiles/bash/config
  . ~/bin/dotfiles/bash/bash_colors.sh
  . ~/bin/dotfiles/bash/aliases
fi

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
