module Cook.Recipe.Util (
    withTempDir
  , getHTTP
  , withFileContent
  , mapFileContent
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Network.HTTP.Simple

import qualified Data.Text.IO as T

import Cook.Recipe

withTempDir :: (FilePath -> Recipe f a) -> Recipe f a
withTempDir recipe = withRecipeName "Util.WithTempDir" $ catchException $ do
    (tmpDir, _) <- runOut $ proc "mktemp" ["--tmpdir", "--directory", "cook-XXXXXX"]
    let tmpDir' = head . lines $ unpack tmpDir
    a <- withCd tmpDir' $ recipe tmpDir'
    runProc "rm" ["-rf", tmpDir']
    return a

getHTTP :: String -> Recipe f ByteString
getHTTP url = withRecipeName "Util.GetHTTP" $ catchException $ do
    req <- parseRequest url
    res <- httpLBS req
    return $ getResponseBody res

withFileContent :: FilePath -> Recipe f Text
withFileContent path = withRecipeName "Util.WithFileContent" $ catchException $ do
    liftIO $ T.readFile path

mapFileContent :: FilePath -> (Text -> Text) -> Recipe f ()
mapFileContent path f = withRecipeName "Util.MapFileContent" $ catchException $ do
    content <- liftIO $ T.readFile path
    liftIO . T.writeFile path $ f content
