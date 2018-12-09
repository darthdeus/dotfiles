#!/bin/bash
set -euxo pipefail

tempdir=$(mktemp -d)

cd /tmp
git clone https://github.com/ggreer/the_silver_searcher.git "$tempdir"
cd "$tempdir"
./build.sh
cp ag ~/.dotfiles/bin/ag
