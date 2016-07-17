module Cook.Recipe.Config (
    loadConfig
  ) where

import Control.Exception
import Data.Aeson
import System.FilePath

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Yaml as Y

loadConfig :: FromJSON a => FilePath -> IO a
loadConfig path
  | takeExtension path == ".json" = loadJSONConfig path
  | takeExtension path == ".yaml" = loadYAMLConfig path
  | otherwise                     = fail "Unsupported config format" -- XXX

loadJSONConfig :: FromJSON a => FilePath -> IO a
loadJSONConfig path = do
    conf <- B.readFile path -- TODO: Catch IO error
    case J.eitherDecode conf of
        Left err  -> fail err -- XXX
        Right obj -> return obj

loadYAMLConfig :: FromJSON a => FilePath -> IO a
loadYAMLConfig path = do
    conf <- B.readFile path -- TODO: Catch IO error
    case Y.decodeEither' (B.toStrict conf) of
        Left err  -> throwIO err -- XXX
        Right obj -> return obj
