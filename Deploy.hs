{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE DeriveFunctor      #-}

module Deploy where

import Control.Monad.IO.Class
import GHC.IO.Handle
import System.Exit

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
newtype StepM a = StepM { runStepM :: IO (a, Maybe Process) } deriving Functor

instance Applicative StepM where
    pure = liftIO . return
    StepM mf <*> StepM mx = StepM $ do
        (f, _) <- mf
        (x, p) <- mx
        return $ (f x, p)

instance Monad StepM where
    -- TODO: with >>= wait and take stdout like (stdout, Process)
    --       with >>  just wait
    StepM ma >>= f = StepM $ ma >>= wait >>= runStepM . f
        where wait (a, Just (i, o, e, h))  = waitForProcess h >> return a
              wait (a, Nothing)            = return a

instance MonadIO StepM where
    liftIO mx = StepM $ mx >>= return . flip (,) Nothing

-- ($|)

type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

noProcess = (Nothing, Nothing, Nothing, Nothing)

withProcess (stdin, stdout, stderr, proc) = (stdin, stdout, stderr, Just proc)

type CRes = Maybe Handle


foo :: ReceiptEnv a a
foo = do Env a <- ask
         return a

data Owner = KeepOwner -- | ...

data Mode = KeepMode -- | ...


data Step' a where
    Proc0 :: FilePath -> Step' CRes
    Proc :: FilePath -> [String] -> Step' CRes
    Sh :: String -> Step' a
    Templ :: (Data a, Typeable a, Generic a, FromJSON a) => FilePath -> FilePath -> Step' a

--runStep' :: forall a. Show a => Step' a -> StepM CRes
--runStep' (Cmd cmd) = liftIO $ callCommand cmd >> return Nothing
--runStep' (Templ src dst) = liftIO $ useTemplate (toTemplate src :: Template a) dst >> return Nothing





runStep :: forall a. Show a => Step' a -> StepM ()
runStep (Sh cmd) = StepM $ (createProcess $ shell cmd) >>= return . (,) () . Just










-----------------------------------------------------------------------------------
{-
 -data Step a = Cp -- XXX
 -          | CpR -- XXX
 -          | Mv -- XXX
 -          | MkDir FilePath
 -          | ChOwnMod Owner Mode
 -          | Templ FilePath FilePath
 -
 -type Recipe a = [Step a]
 -
 -type DirTree = Tree FilePath
 -
 -mkDirTree = undefined
 -
 --- ?
 -templateBase :: IO FilePath
 -templateBase = undefined
 -
 -files =
 -    [ Cp -- "foo.conf" "/etc" DefMode
 -    , Mv -- "foo.conf" "/etc" PreserveMode
 -    , Templ "bar.conf.tmpl" "/etc"
 -    ]
 -
 -
 -
 -runStep :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
 -              => Step a -> IO ()
 -runStep (MkDir dir) = callProcess "mkdir" ["-p", dir]
 -runStep (Templ src dst) = useTemplate (toTemplate src :: Template a) dst
 -
 -runRecipe :: forall a. (Data a, Typeable a, Generic a, FromJSON a, Show a)
 -              => Recipe a -> IO ()
 -runRecipe recp = mapM_ runStep recp
 -
 -
 -resolveSrcPath :: FilePath -> FilePath
 -resolveSrcPath path | isAbsolute path = path
 -                    | otherwise       = "files" </> path
 -}
