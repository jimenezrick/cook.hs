#!/bin/bash

set -euo pipefail

pacman -Syu --quiet --needed --noconfirm git ghc cabal-install

rm -rf cook.hs
git clone https://github.com/jimenezrick/cook.hs.git

pushd cook.hs
cabal update
#cabal install --only-dependencies -j2
cabal install --global -j2
popd
