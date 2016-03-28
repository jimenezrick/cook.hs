module Cook.Catalog.Arch.Pacman (
    upgradeInstalledPackages
  , installPackages
  ) where

import Cook.Recipe

pacman :: [String] -> Recipe ()
pacman args = runProc "pacman" $ ["--quiet", "--noconfirm"] ++ args

upgradeInstalledPackages :: Recipe ()
upgradeInstalledPackages = withRecipeName "Pacman.UpgradeInstalledPackages" $ pacman ["-Syu"]

installPackages :: [String] -> Recipe ()
installPackages pkgs = withRecipeName "Pacman.InstallPackages" $ pacman $ ["--needed", "-S"] ++ pkgs
