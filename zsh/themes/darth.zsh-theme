function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

function current_commit() {
  echo $(git rev-parse --short HEAD 2>/dev/null)
}

ruby_version() {
  echo " $(ruby -v | awk '{print $2}')"
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

local smiley="%(?,%{$fg[green]%}$%{$reset_color%},%{$fg[red]%}$%{$reset_color%})"

local current_dir="%~"

# PROMPT='${current_dir}
PROMPT='%{$fg[blue]%}[%m] ${current_dir}
${smiley} %{$reset_color%}'

RPROMPT='%{$fg[white]%} %{$fg[blue]%}$(current_branch)$(at_sign)%{$fg[yellow]%}$(current_commit) %{$reset_color%}'
