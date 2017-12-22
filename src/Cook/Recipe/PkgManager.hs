module Cook.Recipe.PkgManager
    ( requirePackages
    , upgradePackages
    ) where

import Control.Lens
import Data.List.NonEmpty

import Cook.Recipe
import Cook.Provider

import qualified Cook.Provider.PkgManager as P

requirePackages :: NonEmpty String -> Recipe f ()
requirePackages pkgs = withRecipeName "Recipe.RequirePackages" $ do
    prov <- getProvider
    prov^.pkgManager.P.requirePackages $ pkgs

upgradePackages :: Recipe f ()
upgradePackages = withRecipeName "Recipe.UpgradePackages" $ do
    prov <- getProvider
    prov^.pkgManager.P.upgradePackages
