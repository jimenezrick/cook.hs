module Cook.Catalog.Arch.Pacman
  ( updatePackages
  , upgradePackages
  , clearPackagesCache
  , installPackages
  , provider
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe
import Cook.Provider.PkgManager (Provider)

import qualified Cook.Provider.PkgManager as P

pacman :: NonEmpty String -> Step
pacman args = proc "pacman" $ toList $ ["--quiet", "--noconfirm"] <> args

updatePackages :: Recipe f ()
updatePackages = withRecipeName "Arch.Pacman.UpdatePackages" $
    run $ pacman ["-Sy"]

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Arch.Pacman.UpgradePackages" $
    run $ pacman ["-Syu"]

installPackages :: NonEmpty String -> Recipe f ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $
    run $ pacman $ ["--needed", "-S"] <> pkgs

isPackageInstalled :: String -> Recipe f Bool
isPackageInstalled ""  = error "Pacman.isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "IsPackageInstalled" $ do
    err <- withoutError $ runOut $ pacman ["-Q", pkg]
    either (const $ return False) (const $ return True) err

clearPackagesCache :: Recipe f ()
clearPackagesCache = withRecipeName "Arch.Pacman.ClearPackagesCache" $
    run $ pacman ["-Scc"]

provider :: Provider f
provider = prov
  where prov = P.Provider
            { P._updatePackages     = updatePackages
            , P._upgradePackages    = upgradePackages
            , P._clearPackagesCache = clearPackagesCache
            , P._requirePackages    = P.requirePackagesGeneric prov
            , P._isPackageInstalled = isPackageInstalled
            , P._installPackages    = installPackages
            }
