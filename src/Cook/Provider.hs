module Cook.Provider
    ( Provider
    , provider
    ) where

import Control.Lens

import Cook.Recipe
import Cook.Facts

import qualified Cook.Catalog.Arch.Pacman as A
import qualified Cook.Provider.PkgManager as P

data Provider = Provider
    { _pkgManager :: P.Provider
    }

makeLenses ''Provider

chooseProvider :: Facts a -> Provider
chooseProvider facts = case facts^.systemFacts.osRelease.distro of
                           Arch -> Provider A.provider

provider :: Recipe Provider
provider = undefined -- XXX: Get the provider from the Ctx
