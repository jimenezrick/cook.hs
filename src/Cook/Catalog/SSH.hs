module Cook.Catalog.SSH (
    copyId
  ) where

import Network (HostName)
import Text.Printf

import Cook.Recipe

type UserName = String

copyId :: UserName -> HostName -> Recipe ()
copyId user host = withRecipeName "SSH.CopyId" $ runProc "ssh-copy-id" [target]
  where target = printf "%s@%s" user host
