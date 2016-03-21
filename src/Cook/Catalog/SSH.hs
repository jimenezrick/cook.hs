module Cook.Catalog.SSH (
    copyId
  ) where

import Text.Printf

import Cook.Recipe

copyId :: String -> String -> Recipe ()
copyId user host = withRecipeName "SSH.CopyId" $ runProc "ssh-copy-id" [target]
  where target = printf "%s@%s" user host
