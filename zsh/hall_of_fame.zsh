# This place serves as a retirement home for old aliases.
# Simply deleting them forever is barbaric, but keeping
# them creates clutter.
#
# Another reason for moving them over is that since they
# weren't used in a while, it is quite possible they are
# using obscure/unsupported behavior. But some are kept
# just for fun.
alias reload_webkit="osascript -e \"tell application 'WebKit' to do JavaScript 'window.location.reload()' in front document\""
alias tigs='tig status $argv'

alias b="bundle"
alias r="bundle exec rails"

alias scvload="ssh li 'top -bn 1 | head -n 5'"

alias be="bundle exec"
alias rs="bundle exec rspec spec"
alias rsa="bundle exec rspec spec --only-failures"
alias rb="rbenv"
alias rh="rbenv rehash"

alias rake="bundle exec rake"
alias rspec="bundle exec rspec"

function hc() {
  herbstclient "$@"
}

alias m="mix"
alias mps="mix phoenix.server"
alias im="iex -S mix"
alias is="iex -S mix"
alias isp="iex -S mix phoenix.server"
alias mt="mix test"
alias rdm="rake db:drop db:create db:migrate db:seed"

alias md='kill -s USR1 $(ps -ef | grep main.js | grep node | tr -s " " | cut -f 4 -d " ")'
alias ni="node-inspector --hidden='node_modules' --hidden='node.js' --no-preload"

alias c1="clang++ -std=c++11 -stdlib=libc++"

alias c="cabal"
alias ci="cabal install"
alias cid="cabal install --only-dependencies"
alias cu="cabal update"
alias csi="cabal sandbox init"

alias ct="ctags --extra=+f --language-force=Ruby -R $(bundle show --paths | xargs) app lib"

function switch-gcc-osx() {
  VERSION="${1:-5}"
  export CC="/usr/local/bin/gcc-$VERSION"
  export CXX="/usr/local/bin/g++-$VERSION"
  export CPP="/usr/local/bin/cpp-$VERSION"
  export LD="/usr/local/bin/gcc-$VERSION"
}

function dot-deps() {
  cd "$DOT"

  # TODO: qutebrowser or AUR qutebrowser-git?

  # pacman-key --init
  # pacman-key --populate archlinux

  # gpg --recv-key KEY

  sudo pacman -S --needed $(cat deps)
}

alias z="yaourt"
