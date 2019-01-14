function sa() {
  if [ $# -eq 1 ]; then
    source "$1"/bin/activate
  elif [ -f .venv/bin/activate ]; then
    source .venv/bin/activate
  elif [ -f venv/bin/activate ]; then
    source venv/bin/activate
  fi
}

function de() {
  deactivate
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
  if [ $# -eq 1 ]; then
    virtualenv -p python3 "$1"
  else
    virtualenv -p python3 .venv
  fi
}
