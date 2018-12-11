#!/bin/bash
set -Eeuxo pipefail

if [[ `uname` == 'Linux' ]]; then
  URL="https://github.com/sharkdp/fd/releases/download/v7.2.0/fd-v7.2.0-x86_64-unknown-linux-gnu.tar.gz"
elif [[ `uname` == 'Darwin' ]]; then
  URL="https://github.com/sharkdp/fd/releases/download/v7.2.0/fd-v7.2.0-x86_64-apple-darwin.tar.gz"
fi

hash wget 2>/dev/null || { echo >&2 "wget is missing, aborting"; exit 1; }

wget -O fd.tar.gz "$URL"

mkdir -p fd
tar xf fd.tar.gz -C fd --strip-components=1

cp ./fd/fd ~/.dotfiles/bin/fd

rm fd.tar.gz
rm -rf ./fd
