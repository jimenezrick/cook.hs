module Cook.Catalog.Pacman (
    upgradeInstalledPackages
  , installPackages
  ) where

import Cook.Recipe

type PackageName = String

pacman :: [String] -> Recipe ()
pacman args = runProc "pacman" $ ["--quiet", "--noconfirm"] ++ args

upgradeInstalledPackages :: Recipe ()
upgradeInstalledPackages = withRecipeName "Pacman.UpgradeInstalledPackages" $ pacman ["-Syu"]

installPackages :: [PackageName] -> Recipe ()
installPackages pkgs = withRecipeName "Pacman.InstallPackages" $ pacman $ ["--needed", "-S"] ++ pkgs
