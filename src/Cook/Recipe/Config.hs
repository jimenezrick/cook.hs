module Cook.Recipe.Config (
    loadConfig
  , writeConfig
  , mergeConfig
  , insertConfigWithKey
  , insertConfigInto
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text)
import System.FilePath

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Yaml as Y

import Cook.Recipe

--
-- TODO: Use recipeConfRootDir to load a relative path from there
--

-- FIXME: merge both code paths
loadConfig :: FromJSON a => FilePath -> Recipe a
loadConfig path
  | takeExtension path == ".json" = loadJSONConfig path
  | takeExtension path == ".yaml" = loadYAMLConfig path
  | otherwise                     = failWith "Unsupported config format"

loadJSONConfig :: FromJSON a => FilePath -> Recipe a
loadJSONConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case A.eitherDecode conf of
        Left err  -> failWith err
        Right obj -> return obj

loadYAMLConfig :: FromJSON a => FilePath -> Recipe a
loadYAMLConfig path = do
    conf <- liftIO $ B.readFile path -- TODO: Catch IO error
    case Y.decodeEither' (B.toStrict conf) of
        Left err  -> failWith $ show err
        Right obj -> return obj




mergeConfig :: Value -> Value -> Value
mergeConfig (Object replace) (Object obj) = Object $ H.union replace obj
mergeConfig _           _                 = error "Recipe.Config.mergeConfig: expecting two objects to merge"

insertConfigWithKey :: [Text] -> Value -> Value -> Value
insertConfigWithKey keys v o@(Object _) = o & compose path .~ Just v
  where access k = _Object . at k
        compose  = foldr1 (\f g -> f . _Just . g)
        path     = map access keys
insertConfigWithKey _ _ _ = error "Recipe.Config.insertConfigWithKey: expecting an object to insert a value"

insertConfigInto :: Traversal' Value Value -> Value -> Value -> Value
insertConfigInto place v o@(Object _) = o & place .~ v
insertConfigInto _ _ _ = error "Recipe.Config.insertConfigInto: expecting an object to insert a value"

-- TODO: support Yaml
writeConfig :: ToJSON a => FilePath -> a -> Recipe ()
writeConfig path conf = liftIO $ B.writeFile path $ A.encodePretty' prettyConf conf
  where prettyConf = A.defConfig { A.confCompare = A.compare }
