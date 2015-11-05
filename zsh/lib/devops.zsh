function devops-reload-virtlab() {
  for server in mff-nix-virtlab-qemu-{a-noforward,b,bb,c,cc}; do
    echo "Reloading $server"
    ssh $server "cd .devops; git pull"
  done
}
