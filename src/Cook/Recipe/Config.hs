module Cook.Recipe.Config (
    loadConfig
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import System.FilePath

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Yaml as Y

import Cook.Recipe

--
-- TODO: Use recipeConfRootDir to load a relative path from there
--

loadConfig :: FromJSON a => FilePath -> Recipe a
loadConfig path
  | takeExtension path == ".json" = loadJSONConfig path
  | takeExtension path == ".yaml" = loadYAMLConfig path
  | otherwise                     = failWith "Unsupported config format"

loadJSONConfig :: FromJSON a => FilePath -> Recipe a
loadJSONConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case J.eitherDecode conf of
        Left err  -> failWith err
        Right obj -> return obj

loadYAMLConfig :: FromJSON a => FilePath -> Recipe a
loadYAMLConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case Y.decodeEither' (B.toStrict conf) of
        Left err  -> failWith $ show err
        Right obj -> return obj
