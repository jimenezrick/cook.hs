module Cook.Catalog.Arch.Pacman
  ( upgradePackages
  , clearPackagesCache
  , installPackages
  , provider
  , requirePackages -- XXX
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe
import Cook.Provider.PkgManager (Provider)

import qualified Cook.Provider.PkgManager as P

pacman :: NonEmpty String -> Step
pacman args = proc "pacman" $ toList $ ["--quiet", "--noconfirm"] <> args

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Arch.Pacman.UpgradePackages" $
    run $ pacman ["-Syu"]

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Arch.Pacman.InstallPackages" $
    run $ pacman $ ["--needed", "-S"] <> pkgs

isPackageInstalled :: String -> Recipe Bool
isPackageInstalled ""  = error "Pacman.isPackageInstalled: empty package name"
isPackageInstalled pkg = withRecipeName "IsPackageInstalled" $ do
    err <- withoutError $ runOut $ pacman ["-Q", pkg]
    either (const $ return False) (const $ return True) err

clearPackagesCache :: Recipe ()
clearPackagesCache = withRecipeName "Arch.Pacman.ClearPackagesCache" $
    run $ pacman ["-Scc"]

provider :: Provider
provider = prov
  where prov = P.Provider
            { P.upgradePackages = upgradePackages
            , P.clearPackagesCache = clearPackagesCache
            , P.requirePackages = P.requirePackagesGeneric prov
            , P.isPackageInstalled = isPackageInstalled
            , P.installPackages = installPackages
            }

-- XXX: Hack to let it compile, remove
requirePackages :: NonEmpty String -> Recipe ()
requirePackages = undefined
