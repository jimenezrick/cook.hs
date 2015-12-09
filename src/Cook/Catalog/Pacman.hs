module Cook.Catalog.Pacman (
    pacman
  , upgradePackages
  , installPackages
  ) where

import Cook.Recipe

pacman :: [String] -> Recipe ()
pacman = withRecipeName "Pacman" . pacman'

pacman' :: [String] -> Recipe ()
pacman' args = run $ proc "pacman" $ ["--quiet", "--noconfirm"] ++ args

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Pacman.Upgrade" $ pacman' ["-Syu"]

installPackages :: [String] -> Recipe ()
installPackages pkgs = withRecipeName "Pacman.Install" $ pacman' $ ["--needed", "-S"] ++ pkgs
