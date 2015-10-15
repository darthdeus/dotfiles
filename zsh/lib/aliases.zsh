# Push and pop directories on directory stack
alias pu="pushd"
alias po="popd"

# Basic directory operations
alias ...="cd ../.."
alias -- -="cd -"

# Super user
alias _="sudo"

# Show history
alias history="fc -l 1"

# List direcory contents
alias lsa="ls -lah"
alias l="ls -la"
alias ll="ls -l"
alias sl=ls # often screw this up

alias afind="ack-grep -il"
alias grep="grep --color=auto"


if [ `uname` = 'Darwin' ]; then
  alias ls='ls -G'
  alias du='du -k -d 1 $argv'
else
  alias ls='ls --color=auto'
  alias du='du -k --max-depth=1 $argv'
fi

alias l='ls'
alias ll='ls -lh'
alias la='ls -a'
alias lal='ls -lah'

alias s='git status -sb $argv; return 0'
alias d='gd $argv'
alias m='mate . $argv'

#alias df='df -kh $argv'
alias reload_webkit="osascript -e \"tell application 'WebKit' to do JavaScript 'window.location.reload()' in front document\""
#alias tigs='tig status $argv'

# IP related stuff
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias flush="dscacheutil -flushcache"

alias gzip="gzip -9n"
alias ql="qlmanage -p 2>/dev/null" # preview a file using QuickLook

alias branches='git log --graph --full-history --all --color  --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'

alias mongod="nocorrect mongod"
alias subl="nocorrect subl"
alias vagrant="nocorrect vagrant"

alias tf="tail -F"

alias r="bundle exec rails"
alias rg="rvm gemset"

alias scvload="ssh li 'top -bn 1 | head -n 5'"
alias kapow="touch tmp/restart.txt"

function gdd() {
  git diff $1@{1}..$1
}

alias tmux="tmux -2"
alias t="tmux"
alias ta="tmux attach"
alias tn="tmux new-session -s"

alias be="bundle exec"
alias rs="bundle exec rspec spec"
alias cf="bundle exec cucumber features"
alias rb="rbenv"
alias re="rb"
alias rh="rbenv rehash"
# alias p="sudo -Hiu postgres psql"
alias p="powder"

alias b="bundle"
alias bi="bundle install"

alias wisdom="fortune | cowsay | lolcat"
alias ct='ctags --extra=+f --language-force=Ruby -R $(bundle show --paths | xargs) app lib'

alias md='kill -s USR1 $(ps -ef | grep main.js | grep node | tr -s " " | cut -f 4 -d " ")'
alias ni="node-inspector --hidden='node_modules' --hidden='node.js' --no-preload"
alias im="iex -S mix"

alias c1="clang++ -std=c++11 -stdlib=libc++"

alias c="cabal"
alias ci="cabal install"
alias cid="cabal install --only-dependencies"
alias cu="cabal update"
alias csi="cabal sandbox init"

function pgdisc() {
  echo "SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid();" > psql postgres
}

function z() {
  if [ -S .zeus.sock ]; then
    echo "Zeus is already running"
  else
    zeus start
  fi
}

alias nh="node --harmony"

alias pi="sudo pacman -S"
alias pe="pacman -Ss"
alias pq="pacman -Ssq"

function switch-gcc-osx() {
  VERSION="${1:-5}"
  export CC="/usr/local/bin/gcc-$VERSION"
  export CXX="/usr/local/bin/g++-$VERSION"
  export CPP="/usr/local/bin/cpp-$VERSION"
  export LD="/usr/local/bin/gcc-$VERSION"
}

if ! type open 2>&1 1>/dev/null; then
  alias open="exo-open"
fi

alias pa="perf annotate"
alias pr="perf record -g"
alias pre="perf report -g 'graph,0.5,caller'"
alias relx="xrdb $HOME/.Xresources"
alias chrome="google-chrome --high-dpi-support=1 --force-device-scale-factor=1"

function malloc_dump() {
  LD_PRELOAD="$HOME/.dotfiles/malloc_dump.so" $@
}
