module Cook.Recipe.Util (
    withTempDir
  , getHTTP
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (unpack)
import Network.HTTP.Simple

import Cook.Recipe

withTempDir :: (FilePath -> Recipe a) -> Recipe a
withTempDir recipe = withRecipeName "WithTempDir" $ do
    (tmpDir, _) <- runRead $ proc "mktemp" ["--tmpdir", "--directory", "cook-XXXXXX"]
    let tmpDir' = head . lines $ unpack tmpDir
    a <- withCd tmpDir' $ recipe tmpDir'
    runProc "rm" ["-rf", tmpDir']
    return a

getHTTP :: String -> Recipe ByteString
getHTTP url = withRecipeName "GetHTTP" $ catchException $ do
    req <- parseRequest url
    res <- httpLBS req
    return $ getResponseBody res
