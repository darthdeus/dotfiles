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

alias s='git status -sb'
alias d='gd $argv'

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
alias t="tmux"
alias ta="tmux attach"
alias tn="tmux new-session -s"

alias wisdom="fortune | cowsay | lolcat"

function pgdisc() {
  echo "SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE datname = current_database() AND pid <> pg_backend_pid();" | psql $1
}

alias pi="sudo pacman -S"
alias pe="pacman -Ss"
alias pq="pacman -Ssq"

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

alias ra="ranger --cmd='set show_hidden=true'"
alias rah="ranger"

alias tc="tmux-cssh -cs"

alias dh="du -h | sort -h"

alias dot="cd ~/.dotfiles"

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
alias aur="auracle"
alias aus="auracle search"

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
  conda install scikit-learn future matplotlib numpy scipy pandas pydot graphviz PIL bcolz
  pip install tqdm
}

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

alias jp="jupyter notebook"
alias vis="python -m visdom.server"
alias de="deactivate"

alias pm="python manage.py"
alias pis="pipenv shell"

alias mpo="mrk-proxy-on"
alias mpf="mrk-proxy-off"

alias mmac="source ~/work/mm-backend/.venv/bin/activate"
alias t2="source ~/.venvs/tf2/bin/activate"

alias more="less"

function zman() {
  PAGER="less -g -s '+/^       "$1"'" man zshall
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

function aws-env() {
  access_key="export AWS_ACCESS_KEY_ID=\"$(aws configure get default.aws_access_key_id)\""
  secret_key="export AWS_SECRET_ACCESS_KEY=\"$(aws configure get default.aws_secret_access_key)\""
  region="export AWS_DEFAULT_REGION=\"$(aws configure get default.region)\""

  printf "%s\n%s\n%s\n" "$access_key" "$secret_key" "$region"
}

alias to="vim ~/.todo/main.txt"
