# Installation

First you're going to need to clone this repository to `~/.vim`

    git clone https://github.com/darthdeus/dotvim ~/.vim
    cd ~/.vim
    git submodule update --init --recursive

Symlink the `vimrc`

    ln -nsf ~/.vim/vimrc ~/.vimrc

Install the bundle

    vim +:BundleInstall

Build the clang completer

    cd ~/.vim/bundle/YouCompleteMe
    ./install.sh --clang-completer

# Contributing

There are definitely many things that can be improved. If you can think
of anything, don't hesitate to open an issue or make a pull request.

# License

MIT
