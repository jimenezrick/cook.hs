module Cook.Catalog.Debian.Apt
  ( aptGet
  , addAptRepository
  , updatePackages
  , upgradePackages
  , clearPackagesCache
  , installPackages
  , provider
  ) where

import Data.List (last)
import Data.List.NonEmpty hiding (last)
import Data.Semigroup

import qualified Data.Text.Lazy as T

import Cook.Recipe
import Cook.Provider.PkgManager (Provider)

import qualified Cook.Provider.PkgManager as P

aptGet :: NonEmpty String -> Recipe f ()
aptGet args = withEnv [("DEBIAN_FRONTEND", "noninteractive")] $
    runProc "apt-get" $ toList $ ["--quiet", "--yes"] <> args

addAptRepository :: String -> Recipe f ()
addAptRepository repo = withRecipeName "Debian.Apt.AddAptRepository" $
    runProc "add-apt-repository" [repo]

installPackages :: NonEmpty String -> Recipe f ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $
    aptGet $ "install" <| pkgs

updatePackages :: Recipe f ()
updatePackages = withRecipeName "Debian.Apt.UpdatePackages" $
    aptGet ["update"]

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
    updatePackages
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
    aptGet ["autoremove", "--purge"]
    aptGet ["clean"]

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
