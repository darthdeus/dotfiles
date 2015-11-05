function devops-reload-virtlab() {
  set -A servers mff-nix-virtlab-qemu-{a-noforward,b,bb,c,cc}

  if [ "$1" = "cat" ]; then
    servers+=( cat )
  fi

  if [ "$1" = "cat-only" ]; then
    servers=( cat )
  fi

  for server in $servers; do
    echo "Reloading $server"
    ssh $server "cd .devops; git pull"
  done
}

function devops-setup-help() {
  echo 'sh -c "$(curl -fsSL https://raw.github.com/darthdeus/devops/master/install.sh)"'
}
