module Cook.Catalog.Arch.Pacman (
    upgradeInstalledPackages
  , installPackages
  , isPackageInstalled
  , requirePackages
  ) where

import Control.Monad

import Cook.Recipe

pacman :: [String] -> Recipe ()
pacman args = runProc "pacman" $ ["--quiet", "--noconfirm"] ++ args

upgradeInstalledPackages :: Recipe ()
upgradeInstalledPackages = withRecipeName "Arch.Pacman.UpgradeInstalledPackages" $ pacman ["-Syu"]

installPackages :: [String] -> Recipe ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $ pacman $ ["--needed", "-S"] ++ pkgs

isPackageInstalled :: String -> Recipe Bool
isPackageInstalled pkg = withRecipeName "Arch.Pacman.IsPackageInstalled" $ do
    err <- withoutError $ runRead $ proc "pacman" ["-Qq", pkg]
    either (const $ return False) (const $ return True) err

requirePackages :: [String] -> Recipe ()
requirePackages pkgs = withRecipeName "Arch.Pacman.RequirePackages" $ do
    missingPkgs <- filterM isPackageInstalled pkgs
    installPackages missingPkgs
