#!/usr/bin/env bash
# Format linear-base using the version of Ormolu from the PATH. It's recommended
# using the version provided by `flake.nix`, since it's the one used by the CI
# to enforce formatting.

set -e

export LANG="C.UTF-8"

ormolu -m inplace $(find . -type f -name "*.hs-boot" -o -name "*.hs")
