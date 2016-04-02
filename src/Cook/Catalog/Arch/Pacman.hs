module Cook.Catalog.Arch.Pacman (
    upgradeInstalledPackages
  , installPackages
  , isPackageInstalled
  , requirePackages
  ) where

import Control.Monad
import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe

pacman :: NonEmpty String -> Step
pacman args = proc "pacman" $ toList $ ["--quiet", "--noconfirm"] <> args

upgradeInstalledPackages :: Recipe ()
upgradeInstalledPackages = withRecipeName "Arch.Pacman.UpgradeInstalledPackages" $ do
    run $ pacman ["-Syu"]

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $ do
    run $ pacman $ ["--needed", "-S"] <> pkgs

isPackageInstalled :: String -> Recipe Bool
isPackageInstalled ""  = error "isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "Arch.Pacman.IsPackageInstalled" $ do
    err <- withoutError $ runRead $ pacman ["-Q", pkg]
    either (const $ return False) (const $ return True) err

requirePackages :: NonEmpty String -> Recipe ()
requirePackages pkgs = withRecipeName "Arch.Pacman.RequirePackages" $ do
    missingPkgs <- filterM isPackageInstalled $ toList pkgs
    case missingPkgs of
        [] -> return ()
        _  -> do upgradeInstalledPackages
                 installPackages $ fromList missingPkgs
