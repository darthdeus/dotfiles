# vim :ft=zsh
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_commit() {
  echo $(git rev-parse --short HEAD 2>/dev/null)
}

# ACTUAL CUSTOMIZATION OH NOES!
function minutes_since_last_commit {
    now=`date +%s`
    last_commit=`git log --pretty=format:'%at' -1`
    seconds_since_last_commit=$((now-last_commit))
    minutes_since_last_commit=$((seconds_since_last_commit/60))
    echo $minutes_since_last_commit
}

grb_git_prompt() {
    local g=".git"
    if [ -d "$g" ]; then
        local MINUTES_SINCE_LAST_COMMIT=`minutes_since_last_commit`
        if [ "$MINUTES_SINCE_LAST_COMMIT" -gt 30 ]; then
            local COLOR="$fg[red]"
        elif [ "$MINUTES_SINCE_LAST_COMMIT" -gt 10 ]; then
            local COLOR="$fg[yellow]"
        else
            local COLOR="$fg[green]"
        fi
        local SINCE_LAST_COMMIT="${COLOR}$(minutes_since_last_commit)m${NORMAL}"
        # The __git_ps1 function inserts the current git branch where %s is
        echo $SINCE_LAST_COMMIT
    fi
}

at_sign() {
  git rev-parse --git-dir > /dev/null 2>&1 && echo "@"
}

function git_current_branch() {
  local ref
  ref=$(command git symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(command git rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}


# Gets the number of commits ahead from remote
function git_commits_ahead() {
  if command git rev-parse --git-dir &>/dev/null; then
    git rev-list --count @{upstream}..HEAD
  fi
}

# Gets the number of commits behind remote
function git_commits_behind() {
  if command git rev-parse --git-dir &>/dev/null; then
    git rev-list --count HEAD..@{upstream}
  fi
}

function dotfiles_dirty() {
  DOTFILES_DIRTY='!dot!'
  cd "$HOME/.dotfiles"
  if [[ $(git status --porcelain | wc -l) -ne 0 ]]; then
    echo "$DOTFILES_DIRTY"
  fi
}

# Outputs if current branch is ahead of remote
function git_prompt_ahead() {
  if [[ -n "$(command git rev-list origin/$(git_current_branch)..HEAD 2> /dev/null)" ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_AHEAD"
  fi
}

# Outputs if current branch is behind remote
function git_prompt_behind() {
  if [[ -n "$(command git rev-list HEAD..origin/$(git_current_branch) 2> /dev/null)" ]]; then
    echo "$ZSH_THEME_GIT_PROMPT_BEHIND"
  fi
}

local smiley="%(?,%{$fg[green]%}$%{$reset_color%},%{$fg[red]%}$%{$reset_color%})"

local current_dir="%~"

# PROMPT='%{$fg['
# PROMPT=blue
# PROMPT=']%}[%M] ${current_dir} $fg[red]$(dotfiles_dirty)
# ${smiley} %{$reset_color%}'


# PROMPT='${current_dir}

if [ -f ~/.production ]; then
PROMPT='%{$fg[red]%}[%M] ${current_dir} $fg[red]$(dotfiles_dirty)
${smiley} %{$reset_color%}'
else
PROMPT='%{$fg[blue]%}[%M] ${current_dir} $fg[red]$(dotfiles_dirty)
${smiley} %{$reset_color%}'
fi

RPROMPT='%{$fg[white]%} %{$fg[blue]%}$(current_branch)$(at_sign)%{$fg[yellow]%}$(current_commit) %{$reset_color%}'
