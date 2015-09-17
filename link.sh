#!/bin/sh

DOT="$HOME/.dotfiles"

ln -nsf "$DOT/vim" "$HOME/.vim"
ln -nsf "$DOT/vim/vimrc" "$HOME/.vimrc"

ln -nsf "$DOT/zsh" "$HOME/.zsh"
ln -nsf "$DOT/zsh/zshrc" "$HOME/.zshrc"

ln -nsf "$DOT/emacs.d" "$HOME/.emacs.d"

mkdir -p "$HOME/.ssh"
ln -nsf "$DOT/ssh/config" "$HOME/.ssh/config"

for file in ackrc gemrc irbrc ghci tmux.conf; do
  ln -nsf "$DOT/$file" "$HOME/.$file"
done

XFCE_TERMRC_PATH="$HOME/.config/xfce4/terminal/"
mkdir -p "$XFCE_TERMRC_PATH"
ln -nsf "$HOME/.dotfiles/base16-default.dark.terminalrc" "$XFCE_TERMRC_PATH/terminalrc"
