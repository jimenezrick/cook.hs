#!/bin/bash

base_pkgs=(git ghc cabal-install)
cook_url=https://github.com/jimenezrick/cook.hs.git
cabal_install='--global -j1'

set -euo pipefail

pacman -Syu --quiet --needed --noconfirm ${base_pkgs[*]}

rm -rf cook.hs
git clone $cook_url

pushd cook.hs
cabal update
cabal install $cabal_install
popd
