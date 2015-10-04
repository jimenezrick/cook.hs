{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE DeriveFunctor      #-}

module Deploy where

import Control.Monad.IO.Class
import GHC.IO.Handle

import Data.Data
import GHC.Generics
import Data.Yaml

import Data.Tree
import Control.Monad.Reader
import System.Directory
import System.FilePath
import System.Process

import Template

data Env a = Env a

type ReceiptEnv a = ReaderT (Env a) IO

-- type Step = StepM ()
newtype StepM a = StepM { runStepM :: IO a } deriving Functor

instance Applicative StepM where
    pure = liftIO . return
    StepM mf <*> StepM mx = StepM $ do
        f <- mf
        x <- mx
        return $ f x

instance Monad StepM where
    StepM a >>= f = StepM $ a >>= runStepM . f

instance MonadIO StepM where
    liftIO = StepM


{-type CRes = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)-}
type CRes = Maybe Handle


foo :: ReceiptEnv a a
foo = do Env a <- ask
         return a

data Owner = KeepOwner -- | ...

data Mode = KeepMode -- | ...


data Step' a where
    Cmd' :: String -> Step' a
    Templ' :: (Data a, Typeable a, Generic a, FromJSON a) => FilePath -> FilePath -> Step' a

runStep' :: forall a. Show a => Step' a -> StepM CRes
runStep' (Cmd' cmd) = liftIO $ callCommand cmd >> return Nothing
runStep' (Templ' src dst) = liftIO $ useTemplate (toTemplate src :: Template a) dst >> return Nothing



data Step a = Cp -- XXX
          | CpR -- XXX
          | Mv -- XXX
          | MkDir FilePath
          | ChOwnMod Owner Mode
          | Templ FilePath FilePath
          | Cmd String

type Recipe a = [Step a]

type DirTree = Tree FilePath

mkDirTree = undefined

-- ?
templateBase :: IO FilePath
templateBase = undefined

files =
    [ Cp -- "foo.conf" "/etc" DefMode
    , Mv -- "foo.conf" "/etc" PreserveMode
    , Templ "bar.conf.tmpl" "/etc"
    ]



runStep :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
              => Step a -> IO ()
runStep (MkDir dir) = callProcess "mkdir" ["-p", dir]
runStep (Cmd cmd) = callCommand cmd
runStep (Templ src dst) = useTemplate (toTemplate src :: Template a) dst

runRecipe :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
              => Recipe a -> IO ()
runRecipe recp = mapM_ runStep recp


resolveSrcPath :: FilePath -> FilePath
resolveSrcPath path | isAbsolute path = path
                    | otherwise       = "files" </> path
