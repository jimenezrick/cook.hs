module Cook.Catalog.Ipfs (
    requireIpfs
  ) where

import Control.Monad

import Cook.Recipe
import Cook.Recipe.PkgManager
import Cook.Catalog.Systemd
import Cook.Catalog.User

requireIpfs :: Recipe f ()
requireIpfs = withRecipeName "Ipfs.RequireIpfs" $ do
    requirePackages ["go-ipfs"]
    addUser def "ipfs"
    void $ withSudoUser "ipfs" $ do
        Just home <- getEnv "HOME"
        withCd home $
            withoutError $ runProc "ipfs" ["init"]
    enableService "ipfs"
    startService "ipfs"
