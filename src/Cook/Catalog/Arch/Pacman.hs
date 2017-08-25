module Cook.Catalog.Arch.Pacman (
    upgradePackages
  , installPackages
  , requirePackages
  , provider
  ) where

import Control.Monad
import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe
import Cook.Recipe.Provider.PkgManager

pacman :: NonEmpty String -> Step
pacman args = proc "pacman" $ toList $ ["--quiet", "--noconfirm"] <> args

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Arch.Pacman.UpgradePackages" $ do
    run $ pacman ["-Syu"]

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $ do
    run $ pacman $ ["--needed", "-S"] <> pkgs

isPackageInstalled :: String -> Recipe Bool
isPackageInstalled ""  = error "Pacman.isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "IsPackageInstalled" $ do
    err <- withoutError $ runOut $ pacman ["-Q", pkg]
    either (const $ return False) (const $ return True) err

clearCache :: Recipe ()
clearCache = withRecipeName "Arch.Pacman.ClearCache" $ do
    run $ pacman ["-Scc"]

requirePackages :: NonEmpty String -> Recipe ()
requirePackages pkgs = withRecipeName "Arch.Pacman.RequirePackages" $ do
    missingPkgs <- filterM (fmap not . isPackageInstalled) $ toList pkgs
    case missingPkgs of
        [] -> return ()
        _  -> do
            installPackages $ fromList missingPkgs
            clearCache

provider :: Provider
provider = Provider
    { upgradePackages = upgradePackages
    , isPackageInstalled = isPackageInstalled
    , installPackages = installPackages
    }
