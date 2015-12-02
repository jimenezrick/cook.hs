module Cook.Pacman (
    pacman
  , upgradePackages
  , installPackages
  ) where

import Cook.Recipe

pacman :: [String] -> Recipe ()
pacman args = run $ proc "pacman" $ ["--noconfirm"] ++ args

upgradePackages :: Recipe ()
upgradePackages = pacman ["-Syu"]

installPackages :: [String] -> Recipe ()
installPackages pkgs = pacman $ ["--needed", "-S"] ++ pkgs
