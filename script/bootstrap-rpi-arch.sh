#!/bin/bash

set -euo pipefail

pacman -Syu --quiet --needed --noconfirm git ghc cabal-install

rm -rf cook.hs
git clone https://github.com/jimenezrick/cook.hs.git

pushd cook.hs
cabal update
cabal install --global -j1
popd
