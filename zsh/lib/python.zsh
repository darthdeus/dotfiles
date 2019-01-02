function sa() {
  if [ -f .venv/bin/activate ]; then
    source .venv/bin/activate
  elif [ -f venv/bin/activate ]; then
    source venv/bin/activate
  fi
}

function vpip() {
  if [ -f .venv/bin/pip ]; then
    ./.venv/bin/pip $@
  elif [ -f venv/bin/pip ]; then
    ./venv/bin/pip $@
  else
    echo "virtualenv not found in .venv"
    exit 1
  fi
}

function ve() {
  ~/.local/bin/virtualenv $@
}

# function python() {
#   if [ -f .venv/bin/pip ]; then
#     ./.venv/bin/python $@
#   else
#     command python $@
#   fi
# }
