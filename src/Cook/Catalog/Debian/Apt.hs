module Cook.Catalog.Debian.Apt (
    upgradePackages
  , clearPackagesCache
  , installPackages
  , provider
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe
import Cook.Provider.PkgManager (Provider)

import qualified Cook.Provider.PkgManager as P

aptGet :: NonEmpty String -> Recipe f ()
aptGet args = withEnv [("DEBIAN_FRONTEND", "noninteractive")] $ do
    run $ proc "apt-get" $ toList $ ["--quiet", "--yes"] <> args

installPackages :: NonEmpty String -> Recipe f ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $ do
    aptGet $ "install" <| pkgs

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
    aptGet ["update"]
    aptGet ["upgrade"]

isPackageInstalled :: String -> Recipe f Bool
isPackageInstalled ""  = error "Debian.isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "IsPackageInstalled" $ do
    err <- withoutError $ runOut $ proc "dpkg" ["-l", pkg]
    either (const $ return False) (const $ return True) err

clearPackagesCache :: Recipe f ()
clearPackagesCache = withRecipeName "Debian.Apt.ClearPackagesCache" $ do
    aptGet ["--auto-remove", "purge"]
    aptGet ["clean"]

provider :: Provider f
provider = prov
  where prov = P.Provider
            { P._upgradePackages = upgradePackages
            , P._clearPackagesCache = clearPackagesCache
            , P._requirePackages = P.requirePackagesGeneric prov
            , P._isPackageInstalled = isPackageInstalled
            , P._installPackages = installPackages
            }
