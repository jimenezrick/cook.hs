module Cook.Catalog.Ipfs (
    requireIpfs
  ) where

import Control.Lens
import Control.Monad

import Cook.Provider
import Cook.Recipe
import Cook.Catalog.Systemd
import Cook.Catalog.User

requireIpfs :: Recipe f ()
requireIpfs = withRecipeName "Ipfs.RequireIpfs" $ do
    prov <- getProvider
    prov^.pkgManager.requirePackages $ ["go-ipfs"]
    addUser True [] "ipfs"
    void $ withSudoUser "ipfs" $ do
        Just home <- getEnv "HOME"
        withCd home $
            withoutError $ runProc "ipfs" ["init"]
    enableService "ipfs"
    startService "ipfs"
