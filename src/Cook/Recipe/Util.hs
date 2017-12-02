module Cook.Recipe.Util (
    withTempDir
  , getHTTP
  , withFileContent
  , mapFileContent
  , mapFileLines
  , execCwd
  ) where

import Data.ByteString.Lazy (ByteString ,fromStrict)
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Network.HTTP.Simple

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Cook.Recipe

withTempDir :: (FilePath -> Recipe f a) -> Recipe f a
withTempDir recipe = withRecipeName "Util.WithTempDir" $ do
    (tmpDir, _) <- runOut $ proc "mktemp" ["--tmpdir", "--directory", "cook-XXXXXX"]
    let tmpDir' = head . lines $ unpack tmpDir
    a <- withCd tmpDir' $ recipe tmpDir'
    runProc "rm" ["-rf", tmpDir']
    return a

getHTTP :: String -> Recipe f ByteString
getHTTP url = withRecipeName "Util.GetHTTP" $ do
    req <- recipeIO $ parseRequest url
    res <- recipeIO $ httpLBS req
    return $ getResponseBody res

withFileContent :: FilePath -> Recipe f Text
withFileContent path = withRecipeName "Util.WithFileContent" $
    recipeIO $ T.readFile path

mapFileContent :: FilePath -> (Text -> Text) -> Recipe f ()
mapFileContent path f = withRecipeName "Util.MapFileContent" $ do
    content <- recipeIO $ T.readFile path
    recipeIO . T.writeFile path $ f content

mapFileLines :: FilePath -> ([Text] -> [Text]) -> Recipe f ()
mapFileLines path f = withRecipeName "Util.MapFileLines" $
    mapFileContent path $ T.unlines . f . T.lines

execCwd :: Step -> Recipe f ()
execCwd (Proc prog args) = withRecipeName "Util.ExecCwd" $ do
    -- FIXME: Not supported for Shell
    cwd <- getCwd
    runInB (fromStrict execcwd) $ proc "sh" $ ["-s", cwd, prog] ++ args
  where execcwd = $(embedFile "execcwd.sh")
