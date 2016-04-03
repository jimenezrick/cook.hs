module Cook.Catalog.IPFS (
    requireIPFS
  ) where

import Data.FileEmbed
import Data.Text.Encoding

import Cook.Recipe
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd
import Cook.Catalog.User

requireIPFS :: Recipe ()
requireIPFS = withRecipeName "IPFS.RequireIPFS" $ do
    requirePackages ["go-ipfs"]
    addUser True [] "ipfs"
    withSudoUser "ipfs" $ do
        home <- getEnv "HOME"
        withCd home $ do
            withoutError $ runProc "ipfs" ["init"]
    createFsTree unitsPath unitFile
    enableService "ipfs"
    startService "ipfs"
  where unitsPath = "/usr/lib/systemd/system"
        unitFile  = File "ipfs.service" unit (Nothing, Nothing)
        unit      = Content $ decodeUtf8 $(embedFile "ingredient/systemd/ipfs.service")
