module Cook.Catalog.Debian.Apt (
    upgradePackages
  , installPackages
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe

aptGet :: NonEmpty String -> Recipe ()
aptGet args = withEnv [("DEBIAN_FRONTEND", "noninteractive")] $ do
    run $ proc "apt-get" $ toList $ ["--quiet", "--yes"] <> args

installPackages :: NonEmpty String -> Recipe ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $ do
    aptGet $ "install" <| pkgs

upgradePackages :: Recipe ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
    aptGet $ "update" :| []
    aptGet $ "upgrade" :| []
