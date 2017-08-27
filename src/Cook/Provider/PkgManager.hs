module Cook.Provider.PkgManager
    ( Provider (..)
    , nullProvider
    ) where

import Control.Monad (filterM)
import Data.List.NonEmpty

import Cook.Recipe

data Provider = Provider
    { upgradePackages    :: Recipe ()
    , isPackageInstalled :: String -> Recipe Bool
    , installPackages    :: NonEmpty String -> Recipe ()
    }

{- XXX Implement this generically based in the providers
 -requirePackages :: NonEmpty String -> Recipe ()
 -requirePackages pkgs = withRecipeName "Provider.PkgManager.RequirePackages" $ do
 -    -- TODO: getProvider
 -    missingPkgs <- filterM (fmap not . isPackageInstalled) $ toList pkgs
 -    case missingPkgs of
 -        [] -> return ()
 -        _  -> do
 -            upgradePackages
 -            installPackages $ fromList missingPkgs
 -}

nullProvider :: Provider
nullProvider = Provider
    { upgradePackages = return ()
    , isPackageInstalled = const $ return False
    , installPackages = const $ return ()
    }
