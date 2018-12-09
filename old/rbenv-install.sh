#!/bin/bash

RBENV="$HOME/.rbenv"
RUBY_BUILD="$RBENV/plugins/ruby-build"

if [ -d "$RBENV" ]; then
  cd ~/.rbenv || exit 1
  git pull

  if [ -d "$RUBY_BUILD" ]; then
    cd "$RUBY_BUILD" || exit 1
    git pull
  else
    git clone https://github.com/rbenv/ruby-build.git "$RUBY_BUILD"
  fi
else
  git clone https://github.com/rbenv/rbenv.git "$RBENV"
  git clone https://github.com/rbenv/ruby-build.git "$RUBY_BUILD"
fi
