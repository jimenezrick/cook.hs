module Cook.Recipe.Config (
    ConfigType (..)
  , loadConfig
  , readConfig
  , writeConfig
  , mergeConfig
  , insertConfigWithKey
  , insertConfigInto
  ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson.Lens
import Data.Text (Text)
import Data.ByteString.Lazy (toStrict, fromStrict)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y

import Cook.Recipe

data ConfigType = JSON | YAML

loadConfig :: FromJSON a => ConfigType -> FilePath -> Recipe f a
loadConfig typ path = do
    conf <- recipeIO $ B.readFile path
    readConfig typ $ T.decodeUtf8 conf

readConfig :: FromJSON a => ConfigType -> Text -> Recipe f a
readConfig typ conf =
    case decode typ (T.encodeUtf8 conf) of
        Left err  -> failWith err
        Right obj -> return obj
  where decode JSON = A.eitherDecode . fromStrict
        decode YAML = Y.decodeEither

writeConfig :: ToJSON a => ConfigType -> a -> Recipe f Text
writeConfig typ conf = return . T.decodeUtf8 $ encode typ conf
  where encode JSON = toStrict . A.encodePretty' prettyJSON
        encode YAML = Y.encodePretty prettyYAML
        prettyJSON  = A.defConfig { A.confCompare = A.compare }
        prettyYAML  = Y.setConfCompare A.compare Y.defConfig

mergeConfig :: Value -> Value -> Value
mergeConfig (Object replace) (Object obj) = Object $ H.unionWith ignoreNull replace obj
  where ignoreNull Null v2 = v2
        ignoreNull v1   _  = v1
mergeConfig _ _ = error "Config.mergeConfig: expecting two objects to merge"

insertConfigWithKey :: [Text] -> Value -> Value -> Value
insertConfigWithKey keys v o@(Object _) = o & compose path .~ Just v
  where access k = _Object . at k
        compose  = foldr1 (\f g -> f . _Just . g)
        path     = map access keys
insertConfigWithKey _ _ _ = error "Config.insertConfigWithKey: expecting an object to insert a value"

insertConfigInto :: Traversal' Value Value -> Value -> Value -> Value
insertConfigInto place v o@(Object _) = o & place .~ v
insertConfigInto _ _ _ = error "Config.insertConfigInto: expecting an object to insert a value"
