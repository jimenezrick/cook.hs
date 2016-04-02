module Cook.Catalog.IPFS (
    requireIPFS
  ) where

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
            runProc "ipfs" ["init"]
    createFsTree unitsPath unitFile
    enableService "ipfs"
    startService "ipfs"
  where unitsPath = "/usr/lib/systemd/system"
        unitFile  = File "ipfs.service" unit (Nothing, Nothing)
        unit      = Copy "ingredient/systemd/ipfs.service"
