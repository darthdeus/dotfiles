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

alias bc="bc -l"

alias re="watchexec"
alias r="watchexec"

alias l="ls"
alias ll="ls -lh"
alias la="ls -a"
alias lal="ls -lah"
alias tree="tree -F -a --dirsfirst"

alias s="git status -sb"
alias d="git diff"
alias nx="npx"
alias n="npm"
alias ns="npm run start"
alias ni="npm install"
alias nv="npm version"
alias nr="npm run"
# alias n="newsboat"

alias ag="ag --path-to-ignore ~/.ignore"

alias glsl="glslViewer"

# alias a="pnpm"
alias b="bundler"
alias r="rails"

# IP related stuff
alias myip="dig +short myip.opendns.com @resolver1.opendns.com"
alias flush="dscacheutil -flushcache"

alias gzip="gzip -9n"
alias ql="qlmanage -p 2>/dev/null" # preview a file using QuickLook

alias branches='git log --graph --full-history --all --color  --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'

alias tf="tail -F"

function gdd() {
  git diff $1@{1}..$1
}

alias tmux="tmux -2"
# alias t="tmux"
alias ta="tmux attach"
alias tn="tmux new-session -s"

function pgdisc() {
  echo "SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid();" | psql $1
}

if command -v lsb_release > /dev/null; then
  rel=$(lsb_release -c)
  if [[ $rel == *void* ]]; then
    alias pi="sudo xbps-install -S"
    alias pq="xbps-query -Rs"
    alias pe="xbps-query -R"
  fi
fi

osname=$(uname -a)
if [[ $osname == *gentoo* ]]; then
  alias pi="sudo emerge -av"
  alias pq="eix"
  alias pe="emerge --search"
  alias eu="equery uses"
fi

if [[ $osname == *NixOS* ]]; then
  alias pi="nix-env -i"
  alias pq="cat ~/.nix-pkg-cache | grep"
  alias pe="nix search"
  alias pu="nix-env -v -qaP '*' > ~/.nix-pkg-cache"
  alias n="nix-env"
  alias nc="nix-channel"
  alias ns="nix-shell"
fi

# if [[ $osname == *pipik* ]]; then
# if uname -a | grep -i arch >/dev/null; then
  alias pi="sudo pacman -S"
  alias pe="pacman -Ss"
  alias pq="pacman -Ssq"
# fi

if ! type open 2>&1 1>/dev/null; then
  alias open="exo-open"
fi

alias cl="rlwrap ros -Q run"

alias a="make cmake"

alias pass="gopass"
alias pa="perf annotate"
alias pr="perf record -g"
alias pra="perf record -F 99 -a -g"
alias pre="perf report -g 'graph,0.5,caller'"
alias par="perf stat -ad"
alias relx="xrdb $HOME/.Xresources"
alias chrome="google-chrome-stable --high-dpi-support=1 --force-device-scale-factor=1 --force-dark-mode"

function malloc_dump() {
  LD_PRELOAD="$HOME/.dotfiles/malloc_dump.so" $@
}

alias ra="ranger --cmd='set show_hidden=true'"
alias rah="ranger"

alias tc="tmux-cssh -cs"

alias dh="du -h | sort -h"

alias dot="cd ~/.dotfiles/nvim"

alias ft="ftrace"

function backlight() {
  # TODO: make this resistant to invalid arguments
  echo 'sudo tee /sys/class/backlight/intel_backlight/brightness <<< $1'
}

DOT="$HOME/.dotfiles"

function dot-update() {
  cd "$DOT"

  git submodule update --init --recursive
  ./link.sh
}

alias pw="pkgsearch"
# alias aur="auracle"
# alias aus="auracle search"

function build-ycm() {
  cd "$HOME/.vim/bundle/YouCompleteMe"
  ./install.py --clang-completer
}

function fix-max-inotify() {
  echo fs.inotify.max_user_watches=524288 | sudo tee /etc/sysctl.d/40-max-user-watches.conf
  sudo sysctl --system
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

alias gpg="gpg2"
# alias c="cargo make --makefile Make.toml"
# alias c="cargo build"
alias ce="cargo test"

alias doc="docker"
alias dit="docker run -it"
alias dirt="docker run -it --rm"
alias dc="docker-compose"

alias mutt="neomutt"
alias m="mutt"

function man() {
    env \
    LESS_TERMCAP_mb="$(printf "\e[1;31m")" \
    LESS_TERMCAP_md="$(printf "\e[1;31m")" \
    LESS_TERMCAP_me="$(printf "\e[0m")" \
    LESS_TERMCAP_se="$(printf "\e[0m")" \
    LESS_TERMCAP_so="$(printf "\e[1;44;33m")" \
    LESS_TERMCAP_ue="$(printf "\e[0m")" \
    LESS_TERMCAP_us="$(printf "\e[1;32m")" \
    man "${@}"
}

alias p="python3"

if command -v fdfind >/dev/null; then
  alias fd="fdfind"
fi

alias jp="jupyter notebook"
alias vis="python -m visdom.server"
alias de="deactivate"

alias pm="python manage.py"
alias pis="pipenv shell"

alias more="less"

alias ma="micromamba"

function zman() {
  PAGER="less -g -s '+/^       "$1"'" man zshall
}

function source_conda() {
  eval "$(/home/darth/projects/miniconda3/bin/conda shell.zsh hook)"
}

# function ne() {
#   (cd ~/.dotfiles/nixos/; vim shared.nix)
# }

function ne() {
  (cd ~/.dotfiles/nix; vim home.nix)
}

alias hmcd="cd ~/.dotfiles/nix"

function hr() {
  home-manager switch --flake $HOME/.dotfiles/nix --impure
}

function update-nixos() {
  hmcd && sudo nixos-rebuild switch
}

function no() {
  (cd ~/projects/nixpkgs/nixos; fe)
}

function np() {
  (cd ~/projects/nixpkgs/pkgs; fe)
}

function newflake() {
  cp ~/projects/comfy/{flake.nix,flake.lock,.envrc} .
  git add .
  direnv allow
}

# TODO: scope all under one helper?
function nix-dep() {
  pkg=$(find /nix/store -mindepth 1 -maxdepth 1 -type d | fzf --preview 'nix why-depends /run/current-system {}' --preview-window=down)
  nix why-depends /run/current-system "$pkg"
}

# TODO: put this where it belongs
function after-first-word() {
  zle beginning-of-line
  zle forward-word
}
zle -N after-first-word
bindkey "^X1" after-first-word

function aws-refresh-keys() {
  (cd ~/work/generate-aws-config; ./generate-aws-config --account lab)
}

function port() {
  local dir=`find /var/db/repos/gentoo -mindepth 2 -maxdepth 2 -type d -print 2> /dev/null | fzf --preview 'repo="{}" && eix $(basename "$(dirname "$repo")")/$(basename "$repo")'`
  cd "$dir"
}
