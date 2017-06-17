#!/usr/bin/env sh
set -o errexit -o xtrace

if test ! -f "$HOME/.local/bin/stack"
then
  cd "$(mktemp --directory)"
  curl --location 'https://www.stackage.org/stack/linux-x86_64-static' --output stack.tar.gz
  tar --extract --file stack.tar.gz --strip-components 1
  mkdir --parents "$HOME/.local/bin"
  mv stack "$HOME/.local/bin"
fi
