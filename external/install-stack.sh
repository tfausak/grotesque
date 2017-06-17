#!/usr/bin/env sh
set -o errexit -o xtrace

if test ! -f "$HOME/.local/bin/stack"
then
  curl \
    --location 'https://github.com/commercialhaskell/stack/releases/download/v1.4.0/stack-1.4.0-linux-x86_64.tar.gz' \
    --output stack.tar.gz
  tar -x -f stack.tar.gz --strip-components 1
  mkdir -p "$HOME/.local/bin"
  mv stack "$HOME/.local/bin/"
fi
