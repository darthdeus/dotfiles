# vim :ft=zsh
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_commit() {
  echo $(git rev-parse --short HEAD 2>/dev/null)
}

function minutes_since_last_commit {
    now=$(date +%s)
    last_commit=$(git log --pretty=format:'%at' -1)
    seconds_since_last_commit=$((now-last_commit))
    minutes_since_last_commit=$((seconds_since_last_commit/60))
    echo $minutes_since_last_commit
}

function grb_git_prompt() {
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

        echo $SINCE_LAST_COMMIT
    fi
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


function dotfiles_dirty() {
  if [[ ! -z $(cd "$HOME/.dotfiles" && git status --porcelain) ]]; then
    echo '!dot!'
  fi
}

function at_sign() {
  git rev-parse --git-dir > /dev/null 2>&1 && echo "@"
}

# if [ -f ~/.production ]; then
# ... TODO?
# fi

PROMPT='%F{blue}[%M] %~ %F{red}$(dotfiles_dirty) $(grb_git_prompt)
%(?,%F{green},%F{white}%K{red})%(!,#,\$)%f%k '

RPROMPT='%F{blue}$(current_branch)$(at_sign)%F{yellow}$(current_commit)%f'
