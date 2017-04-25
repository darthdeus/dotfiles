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

alias tf="tail -F"
alias r="bundle exec rails"

alias scvload="ssh li 'top -bn 1 | head -n 5'"

function gdd() {
  git diff $1@{1}..$1
}

alias tmux="tmux -2"
alias t="tmux"
alias ta="tmux attach"
alias tn="tmux new-session -s"

alias be="bundle exec"
alias rs="bundle exec rspec spec"
alias rsa="bundle exec rspec spec --only-failures"
alias rb="rbenv"
alias rh="rbenv rehash"

alias b="bundle"
alias bi="bundle install"

alias wisdom="fortune | cowsay | lolcat"
alias ct='ctags --extra=+f --language-force=Ruby -R $(bundle show --paths | xargs) app lib'

alias md='kill -s USR1 $(ps -ef | grep main.js | grep node | tr -s " " | cut -f 4 -d " ")'
alias ni="node-inspector --hidden='node_modules' --hidden='node.js' --no-preload"

alias c1="clang++ -std=c++11 -stdlib=libc++"

alias c="cabal"
alias ci="cabal install"
alias cid="cabal install --only-dependencies"
alias cu="cabal update"
alias csi="cabal sandbox init"

function pgdisc() {
  echo "SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid();" | psql $1
}

# function z() {
#   if [ -S .zeus.sock ]; then
#     echo "Zeus is already running"
#   else
#     zeus start
#   fi
# }

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
alias pra="perf record -F 99 -a -g"
alias pre="perf report -g 'graph,0.5,caller'"
alias par="perf stat -ad"
alias relx="xrdb $HOME/.Xresources"
alias chrome="google-chrome-stable --high-dpi-support=1 --force-device-scale-factor=1"

function malloc_dump() {
  LD_PRELOAD="$HOME/.dotfiles/malloc_dump.so" $@
}

alias ra="ranger"
alias e="vim"

function hc() {
  herbstclient "$@"
}

alias tc="tmux-cssh -cs"

alias st="foreman start -f Procfile.dev"
alias fst="foreman start -f Procfile.fullDev"
alias stf="foreman start -f Procfile.fullDev"

alias m="mix"
alias mps="mix phoenix.server"
alias im="iex -S mix"
alias is="iex -S mix"
alias isp="iex -S mix phoenix.server"
alias mt="mix test"
alias rdm="rake db:drop db:create db:migrate db:seed"

alias we="webpack"
alias dh="du -h | sort -h"

alias dof="cd ~/.dotfiles"
alias dot="cd ~/.dotfiles"
alias n="npm start"

alias ft="ftrace"

alias rake="bundle exec rake"
alias rspec="bundle exec rspec"
alias rw="rakudobrew"
alias p="perl6"

function backlight() {
  # TODO: make this resistant to invalid arguments
  # TODO: rewrite in perl 6? :)
  echo 'sudo tee /sys/class/backlight/intel_backlight/brightness <<< $1'
}

DOT="$HOME/.dotfiles"

function dot-update() {
  cd "$DOT"

  git submodule update --init --recursive
  ./rbenv-install.sh
  git smart-pull
  ./link.sh
}

function dot-deps() {
  cd "$DOT"

  # TODO: qutebrowser or AUR qutebrowser-git?

  # pacman-key --init
  # pacman-key --populate archlinux

  # gpg --recv-key KEY

  sudo pacman -S --needed $(cat deps)
}

alias pw="pkgsearch"

function build-ycm() {
  cd "$HOME/.vim/bundle/YouCompleteMe"
  ./install.py --system-libclang --clang-completer --gocode-completer
}

function fix-max-inotify() {
  echo fs.inotify.max_user_watches=524288 | sudo tee /etc/sysctl.d/40-max-user-watches.conf
  sudo sysctl --system
}

function ker() {
  cd ~/git/kernels/staging
}

function run-kernel() {
  qemu-system-x86_64 -enable-kvm \
    -cpu host \
    -kernel ./arch/x86/boot/bzImage \
    -append 'console=ttyS0' \
    # -hda disk.img \
    # -initrd /boot/initramfs-linux.img \
    -nographic
}

function suse-deps() {
  sudo zypper in $(cat ~/.dotfiles/suse-deps)
}

function yaourt-install() {
	git clone https://aur.archlinux.org/package-query.git
	cd package-query
	makepkg -si
	cd ..
	git clone https://aur.archlinux.org/yaourt.git
	cd yaourt
	makepkg -si
	cd ..
}

function basic-pip() {
  pip install sklearn future matplotlib numpy scipy pandas
}

alias doc="docker"
alias z="yaourt"
