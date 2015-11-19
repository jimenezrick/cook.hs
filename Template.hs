{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Template where

import Debug.Trace

import Data.Default
import Data.Tagged

import System.FilePath

import GHC.Generics
import Data.Yaml

import Control.Monad.IO.Class
import Data.Data
import Data.Text.Lazy
import Data.Text.Lazy.IO as T
import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text.Lazy.IO as T

type Template a = Tagged a FilePath

toTemplate :: FilePath -> Template a
toTemplate = Tagged

useTemplate :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
            => Template a -> FilePath -> IO ()
useTemplate src dst = do
    vals <- decodeFileEither $ src' -<.> "yaml"
    case vals of
        Left ex -> error $ prettyPrintParseException ex
        Right b -> do traceShowM b
                      let ctx = mkGenericContext (b :: a)
                      hastacheFile conf src' ctx >>= T.writeFile dst
  where conf = hastacheConf $ takeDirectory src'
        src' = untag src

hastacheConf :: FilePath -> MuConfig IO
hastacheConf tmplsPath = defaultConfig { muEscapeFunc = emptyEscape
                                       , muTemplateFileExt = Just ".mustache"
                                       , muTemplateFileDir = Just tmplsPath
                                       }

data SystemConf = SystemConf {
    filesDir  :: FilePath
  , tmplDir   :: FilePath
  , configDir :: FilePath
  } deriving (Show, Data, Typeable, Generic)

instance Default SystemConf where
    def = SystemConf "files" "template" "config"

instance FromJSON SystemConf

loadConf :: forall a. (Default a, Data a, Typeable a, Generic a, FromJSON a, Show a)
         => Template a -> IO (MuContext IO)
loadConf = loadConfWithDef $ Just def

loadConfNoDef :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
              => Template a -> IO (MuContext IO)
loadConfNoDef = loadConfWithDef Nothing

loadConfWithDef :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
         => Maybe a -> Template a -> IO (MuContext IO)
loadConfWithDef withDef path = do
    vals <- decodeFileEither $ path' -<.> "yaml"
    case vals of
        Left ex -> error $ prettyPrintParseException ex
        Right v | Nothing <- withDef -> do traceShowM v
                                           return $ mkGenericContext (v :: a)
                | Just d <- withDef -> do
                    traceShowM v
                    return $ (mkGenericContext (v :: a)) `composeCtx` (mkGenericContext d)
  where conf  = hastacheConf $ takeDirectory path'
        path' = untag path
