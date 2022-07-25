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

# Push and pop directories on directory stack
alias pu="pushd"
alias po="popd"

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

alias wisdom="fortune | cowsay | lolcat"

alias mpo="mrk-proxy-on"
alias mpf="mrk-proxy-off"

alias mmac="source ~/work/mm-backend/.venv/bin/activate"
alias t2="source ~/.venvs/tf2/bin/activate"

function aws-env() {
  access_key="export AWS_ACCESS_KEY_ID=\"$(aws configure get default.aws_access_key_id)\""
  secret_key="export AWS_SECRET_ACCESS_KEY=\"$(aws configure get default.aws_secret_access_key)\""
  region="export AWS_DEFAULT_REGION=\"$(aws configure get default.region)\""

  printf "%s\n%s\n%s\n" "$access_key" "$secret_key" "$region"
}

alias ct="ctags --extra=+f --language-force=Ruby -R $(bundle show --paths | xargs) app lib"

function switch-gcc-osx() {
  VERSION="${1:-5}"
  export CC="/usr/local/bin/gcc-$VERSION"
  export CXX="/usr/local/bin/g++-$VERSION"
  export CPP="/usr/local/bin/cpp-$VERSION"
  export LD="/usr/local/bin/gcc-$VERSION"
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

function dot-deps() {
  cd "$DOT"

  # TODO: qutebrowser or AUR qutebrowser-git?

  # pacman-key --init
  # pacman-key --populate archlinux

  # gpg --recv-key KEY

  sudo pacman -S --needed $(cat deps)
}

alias z="yaourt"

[ -f ~/mrk-proxy/mrk-proxy ] && . ~/mrk-proxy/mrk-proxy
if [ "$(get-ssid)" = "Wireless1" ]; then
  mpo
fi

function get-ssid() {
  # TODO: ubuntu
  local WIFI_CMD=/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport
  if [ -f "$WIFI_CMD" ]; then
    $WIFI_CMD -I | awk '/ SSID/ {print substr($0, index($0, $2))}'
  fi
}

# add-less-suffix() {
#   BUFFER="$BUFFER | less"
#   (( CURSOR += 7 ))
# }
# zle -N add-less-suffix
# bindkey '^I' add-less-suffix

# zstyle :compinstall filename '/Users/darth/.zshrc'
# TODO: create ~/.zfunc if not exists

# TODO: use this instead of custom prompt?
# autoload -U promptinit
# promptinit
# prompt gentoo

# Autoreload all completion commands
# function au() {
#   unfunction -m '_*'
#   autoload -Uz compinit
#   compinit
# }


##################################
# PREVIOUS GPG CONFIG
osname=$(uname -a)
# if [[ ! $osname == *NixOS* ]]; then
#   if command -v gpgconf >/dev/null; then
#     export GPG_TTY="$(tty)"
#     export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
#     gpgconf --launch gpg-agent
#     # TODO: when exactly is this needed?
#     gpg-connect-agent updatestartuptty /bye >/dev/null
#   fi
# fi

##################################

[ -f "$NHM" ] && source "$NHM"

if type direnv &>/dev/null; then
  eval "$(direnv hook zsh)"
fi
# export RA_LOG=rust_analyzer=info


# TODO: defaults write -g com.apple.mouse.scaling -integer -1
# defaults write -g com.apple.scrollwheel.scaling -1

[ -f ~/.env ] && source ~/.env

# NHM="$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
# export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}

unset npm_config_prefix
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm

[ -f /usr/share/nvm/init-nvm.sh ] && source /usr/share/nvm/init-nvm.sh

##########################################
## PROFILING START
if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
  zmodload zsh/zprof
fi

## PROFILING END
if [[ "$PROFILE_ZSH_STARTUP" == true ]]; then
  zprof
fi
##
##########################################

# TODO: store these in a subdir per host
# if [[ "$HOST" == "pipik" ]]; then
#   export QT_SCALE_FACTOR=2
#   export GDK_SCALE=2
# fi

function source-nix() {
  source ~/.nix-profile/etc/profile.d/nix.sh
}

export PROJECT_HOME="$HOME/projects"
