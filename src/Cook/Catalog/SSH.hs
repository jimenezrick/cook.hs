module Cook.Catalog.SSH (
    copyId
  , authorizeKey
  , copyFile
  ) where

import System.FilePath
import Text.Printf

import Cook.Recipe

copyId :: String -> String -> Recipe ()
copyId user host = withRecipeName "SSH.CopyId" $ runProc "ssh-copy-id" [target]
  where target = printf "%s@%s" user host

authorizeKey :: FilePath -> String -> Recipe ()
authorizeKey pubKeyPath user = withRecipeName "SSH.AuthorizeKey" $ do
    createFsTree home tree
  where tree = Dir ".ssh" (Just 0o700, Just (user, user)) [
                   File "authorized_keys" key (Just 0o600, Just (user, user))
               ]
        key  = Copy pubKeyPath
        home = "/home" </> user

copyFile :: String -> String -> FilePath -> FilePath -> Recipe()
copyFile user host src dst = runProc "scp" [src, printf "%s@%s:%s" user host dst]
