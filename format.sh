#!/usr/bin/env bash
# Format linear-base using the version of Ormolu specified in stack.yaml.

set -e

export LANG="C.UTF-8"

stack build ormolu
## We can't format cabal at the moment because `cabal format` inlines
## common stanzas, which is very much something that we don't want. See
## https://github.com/haskell/cabal/issues/5734
#
# cabal format
stack exec ormolu -- -m inplace $(find . -type f -name "*.hs-boot" -o -name "*.hs")
