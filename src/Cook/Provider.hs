module Cook.Provider
    ( Provider
    , pkgManager

    , getProvider
    ) where

import Control.Lens

import Cook.Provider.PkgManager hiding (Provider)
import Cook.Recipe (Recipe, getFacts)
import Cook.Facts

import qualified Cook.Catalog.Arch.Pacman as A
import qualified Cook.Catalog.Debian.Apt as D
import qualified Cook.Provider.PkgManager as P

data Provider f = Provider
    { _pkgManager :: P.Provider f
    }

makeLenses ''Provider

chooseProvider :: Facts f -> Provider f
chooseProvider facts = case facts^.systemFacts.osRelease.distro of
                           Arch   -> Provider A.provider
                           Debian -> Provider D.provider
                           Ubuntu -> Provider D.provider

getProvider :: Recipe f (Provider f)
getProvider = chooseProvider <$> getFacts
