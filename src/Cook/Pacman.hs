module Cook.Pacman (
    pacman
  , upgradePackages
  , installPackages
  ) where

import Cook.Recipe

pacman :: [String] -> Step
pacman args = proc "pacman" $ ["--noconfirm"] ++ args

upgradePackages :: Step
upgradePackages = pacman ["-Syu"]

installPackages :: [String] -> Step
installPackages pkgs = pacman $ ["--needed", "-S"] ++ pkgs
