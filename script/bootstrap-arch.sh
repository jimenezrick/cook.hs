#!/bin/bash

base_pkgs=(git ghc cabal-install)
cook_url=https://github.com/jimenezrick/cook.hs.git
cabal_flags='--global -j1'

set -euo pipefail

die() {
	echo "$@" >&2
	exit 1
}

(( EUID == 0 )) || die 'This script must be run with root privileges'

pacman -Syu --quiet --needed --noconfirm ${base_pkgs[*]}

rm -rf cook.hs
git clone $cook_url

pushd cook.hs
cabal update
cabal install $cabal_flags
popd
