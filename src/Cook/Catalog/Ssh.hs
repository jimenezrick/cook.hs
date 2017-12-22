module Cook.Catalog.Ssh
  ( copyId
  , authorizeKey
  , authorizeKeyFile
  , copyFile
  ) where

import Data.ByteString (ByteString)
import System.FilePath
import Text.Printf

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as T

import Cook.Recipe

copyId :: String -> String -> Recipe f ()
copyId user host = withRecipeName "Ssh.CopyId" $ runProc "ssh-copy-id" [target]
  where target = printf "%s@%s" user host

authorizeKey :: ByteString -> String -> Recipe f ()
authorizeKey pubKey user = withRecipeName "Ssh.AuthorizeKey" $
    createFsTree home tree
  where tree = Dir ".ssh" (Just 0o700, Just (user, user)) [
                   File "authorized_keys" key (Just 0o600, Just (user, user))
               ]
        key  = Content $ T.decodeUtf8 pubKey
        home = case user of
                   "root" -> "/root"
                   _      -> "/home" </> user

authorizeKeyFile :: FilePath -> String -> Recipe f ()
authorizeKeyFile path user = withRecipeName "Ssh.AuthorizeKeyFile" $ do
    key <- recipeIO $ B.readFile path
    authorizeKey key user

copyFile :: String -> String -> FilePath -> FilePath -> Recipe f ()
copyFile user host src dst = withRecipeName "Ssh.CopyFile" $
    runProc "scp" [src, printf "%s@%s:%s" user host dst]
