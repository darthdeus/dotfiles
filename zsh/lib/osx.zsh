# TODO: figure out how to make fasd work with my current hotkeys
# eval "$(fasd --init auto)"

# TODO: fix the OS X gpg agent
# platform=`uname`
# if [[ "$platform" == "Darwin" ]]; then
#   if [ -f ~/.gnupg/.gpg-agent-info  ] && [ -n "$(pgrep gpg-agent)"  ]; then
#     source ~/.gnupg/.gpg-agent-info
#     export GPG_AGENT_INFO
#   else
#     eval $(gpg-agent --daemon --write-env-file ~/.gnupg/.gpg-agent-info)
#   fi
# fi