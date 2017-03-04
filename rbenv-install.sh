#!/bin/bash

RBENV="$HOME/.rbenv"
RUBY_BUILD="$RBENV/plugins/ruby-build"

if [ -d "$RBENV" ]; then
  cd ~/.rbenv
  git pull

  if [ -d "$RUBY_BUILD" ]; then
    cd "$RUBY_BUILD"
    git pull
  else
    git clone https://github.com/rbenv/ruby-build.git "$RUBY_BUILD"
  fi
else
  git clone https://github.com/rbenv/rbenv.git "$RBENV"
  git clone https://github.com/rbenv/ruby-build.git "$RUBY_BUILD"
fi
