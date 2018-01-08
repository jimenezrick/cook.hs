module Cook.Catalog.Debian.Apt (
    upgradePackages
  , clearPackagesCache
  , installPackages
  , provider
  ) where

import Data.List (last)
import Data.List.NonEmpty hiding (last)
import Data.Semigroup

import Cook.Recipe
import Cook.Provider.PkgManager (Provider)

import qualified Data.Text.Lazy as T

import qualified Cook.Provider.PkgManager as P

aptGet :: NonEmpty String -> Recipe f ()
aptGet args = withEnv [("DEBIAN_FRONTEND", "noninteractive")] $
    run $ proc "apt-get" $ toList $ ["--quiet", "--yes"] <> args

installPackages :: NonEmpty String -> Recipe f ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $
    aptGet $ "install" <| pkgs

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
    aptGet ["update"]
    aptGet ["upgrade"]

isPackageInstalled :: String -> Recipe f Bool
isPackageInstalled ""  = error "Debian.isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "IsPackageInstalled" $ do
    res <- withoutError $ runOut $ proc "dpkg" ["-l", pkg]
    case res of
        Left _         -> return False
        Right (out, _) -> return . T.isPrefixOf "ii" . last $ T.lines out

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
