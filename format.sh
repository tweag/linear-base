#!/usr/bin/env bash
# Format linear-base using the version of Ormolu specified in stack.yaml.

set -e

export LANG="C.UTF-8"

stack build ormolu
cabal format
stack exec ormolu -- -m inplace $(find . -type f -name "*.hs-boot" -o -name "*.hs")
