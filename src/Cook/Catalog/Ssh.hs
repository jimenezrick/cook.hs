module Cook.Catalog.Ssh (
    copyId
  , authorizeKey
  , copyFile
  ) where

import System.FilePath
import Text.Printf

import Cook.Recipe

copyId :: String -> String -> Recipe f ()
copyId user host = withRecipeName "Ssh.CopyId" $ runProc "ssh-copy-id" [target]
  where target = printf "%s@%s" user host

authorizeKey :: FilePath -> String -> Recipe f ()
authorizeKey pubKeyPath user = withRecipeName "Ssh.AuthorizeKey" $ do
    createFsTree home tree
  where tree = Dir ".ssh" (Just 0o700, Just (user, user)) [
                   File "authorized_keys" key (Just 0o600, Just (user, user))
               ]
        key  = Copy pubKeyPath
        home = "/home" </> user

copyFile :: String -> String -> FilePath -> FilePath -> Recipe f ()
copyFile user host src dst = runProc "scp" [src, printf "%s@%s:%s" user host dst]
