ZSH=$HOME/.zsh

# history config
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# disable the annoying beep
unsetopt beep

# emacs style keybindings
bindkey -e

# Load all libs under
for config_file ($ZSH/lib/*.zsh) source $config_file

# completion settings
# zstyle :compinstall filename '/Users/darth/.zshrc'
autoload -Uz compinit
compinit

# color theme
source $ZSH/themes/darth.zsh-theme

PATH="$HOME/opt/cross/bin:$PATH"
PATH="$HOME/projects/depot_tools:$PATH"
PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"

export EDITOR="vim"

PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:$PATH"
PATH="$HOME/opt/cross/bin:$HOME/.rbenv/bin:$PATH"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/base16-tomorrow.dark.sh"
[[ -s $BASE16_SHELL ]] && source $BASE16_SHELL

source $ZSH/custom/aliases.zsh
source $ZSH/custom/git_aliases.zsh
