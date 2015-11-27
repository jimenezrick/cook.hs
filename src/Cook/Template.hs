module Cook.Template (
    Template

  , asTemplate
  , useTemplate
  ) where

import Data.Data
import Data.Default
import Data.Monoid
import Data.Tagged
import Data.Yaml
import GHC.Generics
import System.FilePath
import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text.Lazy.IO as Text

type Template a = Tagged a FilePath

asTemplate :: FilePath -> Template a
asTemplate = Tagged

useTemplate :: forall a. (Data a, Typeable a, Generic a, FromJSON a)
            => Template a -> FilePath -> IO ()
useTemplate src dst = do
    vals <- decodeFileEither $ src' -<.> "yaml"
    case vals of
        Left ex -> error $ prettyPrintParseException ex
        Right b -> let ctx = mkGenericContext (b :: a)
                   in hastacheFile conf src' ctx >>= Text.writeFile dst
  where conf = hastacheConf $ takeDirectory src'
        src' = untag src















--------------------------------------------------------
hastacheConf :: FilePath -> MuConfig IO
hastacheConf tmplsPath = defaultConfig { muEscapeFunc = emptyEscape
                                       , muTemplateFileExt = Just ".mustache"
                                       , muTemplateFileDir = Just tmplsPath
                                       }

data SystemConf = SystemConf {
    filesDir  :: FilePath
  , tmplDir   :: FilePath
  , configDir :: FilePath
  } deriving (Data, Typeable, Generic)

instance Default SystemConf where
    def = SystemConf "files" "template" "config"

instance FromJSON SystemConf

loadConf :: forall a. (Default a, Data a, Typeable a, Generic a, FromJSON a)
         => Template a -> IO (MuContext IO)
loadConf = loadConfWithDef $ mkGenericContext (def :: a)

loadConfNoDef :: forall a. (Data a, Typeable a, Generic a, FromJSON a)
              => Template a -> IO (MuContext IO)
loadConfNoDef = loadConfWithDef mempty

loadConfWithDef :: forall a. (Data a, Typeable a, Generic a, FromJSON a)
                => MuContext IO -> Template a -> IO (MuContext IO)
loadConfWithDef withDef path = do
    vals <- decodeFileEither $ path' -<.> "yaml"
    case vals of
        Left ex -> error $ prettyPrintParseException ex
        Right v -> return $ (mkGenericContext (v :: a)) <> withDef
  where conf  = hastacheConf $ takeDirectory path'
        path' = untag path

--------------------------------------------------------
-- data Foo = Foo {
--     foo  :: String
--   , bar   :: Int
--   , bur :: Int
--   } deriving (Data, Typeable, Generic)
-- 
-- instance FromJSON Foo
-- 
-- main :: IO ()
-- main = useTemplate (asTemplate "p.mustache" :: Template Foo) "p.out2"
