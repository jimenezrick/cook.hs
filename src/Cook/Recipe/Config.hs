module Cook.Recipe.Config (
    loadConfig
  , writeConfig
  , mergeConfig
  , insertConfigWithKey
  , insertConfigInto
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON, Value (..))
import Data.Aeson.Lens
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as Y

import Cook.Recipe

data ConfigType = JSON | YAML

loadConfig :: FromJSON a => ConfigType -> FilePath -> Recipe a
loadConfig typ path = catchException $ do
    conf <- liftIO $ B.readFile path
    case decode typ conf of
        Left err  -> failWith err
        Right obj -> return obj
  where decode JSON = A.eitherDecode
        decode YAML = Y.decodeEither . B.toStrict

writeConfig :: ToJSON a => ConfigType -> a -> Recipe Text
writeConfig typ conf = return . T.decodeUtf8 $ encode typ conf
  where encode JSON = B.toStrict . A.encodePretty' prettyJSON
        encode YAML = Y.encodePretty prettyYAML
        prettyJSON  = A.defConfig { A.confCompare = A.compare }
        prettyYAML  = Y.setConfCompare A.compare Y.defConfig

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
