module Cook.Recipe.Util (
    withTempDir
  , getHTTP
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (unpack)
import Network.HTTP.Simple

import Cook.Recipe

withTempDir :: Recipe a -> Recipe a
withTempDir recipe = do
    (tmpDir, _) <- runRead $ proc "mktemp" ["-d", "cook-XXXXXX"]
    let tmpDir' = unpack tmpDir
    a <- withCd tmpDir' recipe
    runProc "rm" ["-rf", tmpDir']
    return a

getHTTP :: String -> Recipe ByteString
getHTTP url = catchException $ do
    req <- parseRequest url
    res <- httpLBS req
    return $ getResponseBody res
