module Cook.Catalog.IPFS (
    requireIPFS
  ) where

import Control.Monad

import Cook.Recipe
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd
import Cook.Catalog.User

requireIPFS :: Recipe ()
requireIPFS = withRecipeName "IPFS.RequireIPFS" $ do
    requirePackages ["go-ipfs"]
    addUser True [] "ipfs"
    void $ withSudoUser "ipfs" $ do
        home <- getEnv "HOME"
        withCd home $ do
            withoutError $ runProc "ipfs" ["init"]
    enableService "ipfs"
    startService "ipfs"
