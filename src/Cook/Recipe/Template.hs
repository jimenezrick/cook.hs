module Cook.Recipe.Template (
    TemplateConf

  , asTemplate

  , useTemplate
  , useTemplateNoDef
  , useTemplateWithDef

  , loadConf
  , loadConfNoDef
  , loadConfWithDef
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

type TemplateConf a = Tagged a FilePath

asTemplate :: FilePath -> TemplateConf a
asTemplate = Tagged

useTemplate :: forall a. (Default a, Data a, Generic a, FromJSON a)
            => TemplateConf a -> FilePath -> IO ()
useTemplate src dst = loadConf src >>= hastacheFile conf src' >>= Text.writeFile dst
  where conf = hastacheConf $ takeDirectory src'
        src' = untag src

useTemplateNoDef :: forall a. (Data a, Generic a, FromJSON a)
                 => TemplateConf a -> FilePath -> IO ()
useTemplateNoDef src dst = loadConfNoDef src >>= hastacheFile conf src' >>= Text.writeFile dst
  where conf = hastacheConf $ takeDirectory src'
        src' = untag src

useTemplateWithDef :: forall a. (Data a, Generic a, FromJSON a)
                   => a -> TemplateConf a -> FilePath -> IO ()
useTemplateWithDef withDef src dst = loadConfWithDef withDef src >>= hastacheFile conf src' >>= Text.writeFile dst
  where conf = hastacheConf $ takeDirectory src'
        src' = untag src

loadConf :: forall a. (Default a, Data a, Generic a, FromJSON a)
         => TemplateConf a -> IO (MuContext IO)
loadConf = loadConf' $ mkGenericContext (def :: a)

loadConfNoDef :: forall a. (Data a, Generic a, FromJSON a)
              => TemplateConf a -> IO (MuContext IO)
loadConfNoDef = loadConf' mempty

loadConfWithDef :: forall a. (Data a, Generic a, FromJSON a)
                => a -> TemplateConf a -> IO (MuContext IO)
loadConfWithDef withDef = loadConf' $ mkGenericContext withDef

loadConf' :: forall a. (Data a, Generic a, FromJSON a)
          => MuContext IO -> TemplateConf a -> IO (MuContext IO)
loadConf' withDef path = do
    vals <- decodeFileEither $ untag path -<.> "yaml"
    case vals of
        Left ex -> error $ prettyPrintParseException ex
        Right v -> return $ mkGenericContext (v :: a) <> withDef

hastacheConf :: FilePath -> MuConfig IO
hastacheConf tmplsPath = defaultConfig {
    muEscapeFunc = emptyEscape
  , muTemplateFileExt = Just ".mustache"
  , muTemplateFileDir = Just tmplsPath
  }
