module Cook.Provider.PkgManager
    ( Provider (..)
    , upgradePackages
    , clearPackagesCache
    , requirePackages
    , isPackageInstalled
    , installPackages

    , requirePackagesGeneric
    , nullProvider
    ) where

import Control.Lens
import Control.Monad (filterM)
import Data.List.NonEmpty

import Cook.Recipe

data Provider f = Provider
    { _upgradePackages    :: Recipe f ()
    , _clearPackagesCache :: Recipe f ()
    , _requirePackages    :: NonEmpty String -> Recipe f ()
    , _isPackageInstalled :: String -> Recipe f Bool
    , _installPackages    :: NonEmpty String -> Recipe f ()
    }

makeLenses ''Provider

requirePackagesGeneric :: Provider f -> NonEmpty String -> Recipe f ()
requirePackagesGeneric prov pkgs = withRecipeName "Provider.RequirePackagesGeneric" $ do
    missingPkgs <- filterM (fmap not . (prov^.isPackageInstalled)) $ toList pkgs
    case missingPkgs of
        [] -> return ()
        _  -> do
            prov^.installPackages $ fromList missingPkgs
            prov^.clearPackagesCache

nullProvider :: Provider f
nullProvider = Provider
    { _upgradePackages = return ()
    , _clearPackagesCache = return ()
    , _requirePackages = const $ return ()
    , _isPackageInstalled = const $ return False
    , _installPackages = const $ return ()
    }
