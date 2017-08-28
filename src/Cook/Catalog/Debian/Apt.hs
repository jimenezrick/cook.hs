module Cook.Catalog.Debian.Apt (
    upgradePackages
  , installPackages
  , requirePackages
  ) where

import Data.List.NonEmpty
import Data.Semigroup

import Cook.Recipe

aptGet :: NonEmpty String -> Recipe f ()
aptGet args = withEnv [("DEBIAN_FRONTEND", "noninteractive")] $ do
    run $ proc "apt-get" $ toList $ ["--quiet", "--yes"] <> args

clearCache :: Recipe f ()
clearCache = withRecipeName "Debian.Apt.ClearCache" $ do
    aptGet ["--auto-remove", "purge"]
    aptGet ["clean"]

installPackages :: NonEmpty String -> Recipe f ()
installPackages pkgs = withRecipeName "Debian.Apt.InstallPackages" $ do
    aptGet $ "install" <| pkgs

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Debian.Apt.UpgradePackages" $ do
    aptGet ["update"]
    aptGet ["upgrade"]

requirePackages :: NonEmpty String -> Recipe f ()
requirePackages pkgs = withRecipeName "Debian.Apt.RequirePackages" $ do
    installPackages pkgs
    clearCache
