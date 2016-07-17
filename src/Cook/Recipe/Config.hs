module Cook.Recipe.Config (
    loadConfig
  ) where

import Control.Monad.IO.Class
import System.FilePath

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Yaml as Y

import Cook.Recipe

type Config = J.Object

loadConfig :: FilePath -> Recipe Config
loadConfig path
  | takeExtension path == ".json" = loadJSONConfig path
  | takeExtension path == ".yaml" = loadYAMLConfig path
  | otherwise                     = failWith "Unsupported config format"

loadJSONConfig :: FilePath -> Recipe Config
loadJSONConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case J.eitherDecode conf of
        Left err  -> failWith err
        Right obj -> return obj

loadYAMLConfig :: FilePath -> Recipe Config
loadYAMLConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case Y.decodeEither (B.toStrict conf) of
        Left err  -> failWith err
        Right obj -> return obj
