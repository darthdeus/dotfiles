#!/bin/sh

git submodule update --init --recursive

DOT="$HOME/.dotfiles"
THIRD_PARTY="$DOT/third-party"

ln -nsf "$DOT/vim"           "$HOME/.vim"
ln -nsf "$DOT/vim/vimrc"     "$HOME/.vimrc"
ln -nsf "$DOT/spacemacs.el"  "$HOME/.spacemacs"
ln -nsf "$DOT/zsh"           "$HOME/.zsh"
ln -nsf "$DOT/zsh/zshrc"     "$HOME/.zshrc"
ln -nsf "$DOT/emacs.d"       "$HOME/.emacs.d"
ln -nsf "$DOT/xmonad"        "$HOME/.xmonad"
ln -nsf "$DOT/Xresources"    "$HOME/.Xresources"
ln -nsf "$DOT/xinitrc"       "$HOME/.xinitrc"
ln -nsf "$DOT/mutt/mailcap"  "$HOME/.mailcap"
ln -nsf "$DOT/msmtprc"       "$HOME/.msmtprc"
ln -nsf "$DOT/offlineimaprc" "$HOME/.offlineimaprc"
ln -nsf "$DOT/mbsyncrc"      "$HOME/.mbsyncrc"

mkdir -p "$HOME/.mutt"
ln -nsf "$DOT/mutt/muttrc"   "$HOME/.mutt/muttrc"
ln -nsf "$DOT/mutt/gmail.rc" "$HOME/.mutt/gmail.rc"
ln -nsf "$DOT/mutt/icloud.rc" "$HOME/.mutt/icloud.rc"

ln -nsf "$DOT/dot-update" "$DOT/bin/dot-update"
ln -nsf "$DOT/sshget"     "$DOT/bin/sshget"
ln -nsf "$DOT/gbrt"       "$DOT/bin/gbrt"

ln -nsf "$DOT/c_ycm_extra_conf.py" "$HOME/.ycm_extra_conf.py"
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

mkdir -p "$HOME/.mail"
mkdir -p "$HOME/.mail/sent"
mkdir -p "$HOME/.mail/drafts"
mkdir -p "$HOME/.mail/icloud"
mkdir -p "$HOME/.mail/gmail"

mkdir -p "$HOME/.config"
mkdir -p "$HOME/.config/nvim"
ln -nsf "$DOT/vim/init.vim" "$HOME/.config/nvim/init.vim"

mkdir -p "$HOME/.ssh"
ln -nsf "$DOT/ssh/config" "$HOME/.ssh/config"

for file in ackrc gemrc irbrc ghci tmux.conf; do
  ln -nsf "$DOT/$file" "$HOME/.$file"
done

XFCE_TERMRC_PATH="$HOME/.config/xfce4/terminal/"
mkdir -p "$XFCE_TERMRC_PATH"
ln -nsf "$HOME/.dotfiles/base16-default.dark.terminalrc" "$XFCE_TERMRC_PATH/terminalrc"

ln -nsf "$HOME/.dotfiles/git/ignore" "$HOME/.gitignore"
# ln -nsf "$HOME/.dotfiles/git/config" "$HOME/.gitconfig"
cp "$HOME/.dotfiles/git/config" "$HOME/.gitconfig"

mkdir -p "$HOME/.config/herbstluftwm"
ln -nsf "$HOME/.dotfiles/herbstluftwm" "$HOME/.config/herbstluftwm/autostart"

ln -nsf "$DOT/Xmodmap" "$HOME/.Xmodmap"
ln -nsf "$DOT/tmux-cssh" "$HOME/.tmux-cssh"

mkdir -p "$HOME/.tmux/plugins"
ln -nsf "$THIRD_PARTY/tpm"           "$HOME/.tmux/plugins/tpm"
ln -nsf "$THIRD_PARTY/rakudobrew"    "$HOME/.rakudobrew"
ln -nsf "$THIRD_PARTY/base16-shell"  "$HOME/.config/base16-shell"

