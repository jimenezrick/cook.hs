module Cook.Recipe.Util (
    withTempDir
  , getHTTP
  , withFileContent
  ) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Network.HTTP.Simple

import qualified Data.Text.IO as T

import Cook.Recipe

withTempDir :: (FilePath -> Recipe a) -> Recipe a
withTempDir recipe = withRecipeName "Util.WithTempDir" $ catchException $ do
    (tmpDir, _) <- runOut $ proc "mktemp" ["--tmpdir", "--directory", "cook-XXXXXX"]
    let tmpDir' = head . lines $ unpack tmpDir
    a <- withCd tmpDir' $ recipe tmpDir'
    runProc "rm" ["-rf", tmpDir']
    return a

getHTTP :: String -> Recipe ByteString
getHTTP url = withRecipeName "Util.GetHTTP" $ catchException $ do
    req <- parseRequest url
    res <- httpLBS req
    return $ getResponseBody res

withFileContent :: FilePath -> (Text -> Text) -> Recipe ()
withFileContent path f = withRecipeName "Util.WithFileContent" $ catchException $ do
    content <- liftIO $ T.readFile path
    liftIO . T.writeFile path $ f content
