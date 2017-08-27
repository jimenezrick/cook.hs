module Cook.Provider.PkgManager
    ( Provider (..)
    , requirePackagesGeneric
    , nullProvider
    ) where

import Control.Monad (filterM)
import Data.List.NonEmpty

import Cook.Recipe

data Provider = Provider
    { upgradePackages    :: Recipe ()
    , clearPackagesCache :: Recipe ()
    , requirePackages    :: NonEmpty String -> Recipe ()
    , isPackageInstalled :: String -> Recipe Bool
    , installPackages    :: NonEmpty String -> Recipe ()
    }

requirePackagesGeneric :: Provider -> NonEmpty String -> Recipe ()
requirePackagesGeneric prov pkgs = withRecipeName "Provider.RequirePackagesGeneric" $ do
    missingPkgs <- filterM (fmap not . isPackageInstalled prov) $ toList pkgs
    case missingPkgs of
        [] -> return ()
        _  -> do
            installPackages prov $ fromList missingPkgs
            clearPackagesCache prov

nullProvider :: Provider
nullProvider = Provider
    { upgradePackages = return ()
    , clearPackagesCache = return ()
    , requirePackages = const $ return ()
    , isPackageInstalled = const $ return False
    , installPackages = const $ return ()
    }
