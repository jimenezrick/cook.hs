{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE DeriveFunctor      #-}

module Deploy where

import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as T

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

type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

newtype StepM a = StepM {
    unStepM :: IO (a, Maybe Process)
  } deriving Functor

instance Applicative StepM where
    pure = liftIO . return
    StepM mf <*> StepM ma = StepM $ do
        (f, _) <- mf
        (a, p) <- ma
        return $ (f a, p)

instance Monad StepM where
    s >>= f = StepM $ runStepM s >>= unStepM . f

instance MonadIO StepM where
    liftIO ma = StepM $ ma >>= return . flip (,) Nothing

runStepM :: StepM a -> IO a
runStepM (StepM ma) = do
    (a, p) <- ma
    case p of
        Just (_, _, _, h) -> waitForProcess h >> return a
        Nothing           -> return a

{-data Env a = Env a-}
{-type ReceiptEnv a = ReaderT (Env a) IO-}

{-
 -foo :: ReceiptEnv a a
 -foo = do Env a <- ask
 -         return a
 -}

data Owner = KeepOwner -- | ...

data Mode = KeepMode -- | ...


data Step' a where
    {-Proc0 :: FilePath -> Step' CRes-}
    {-Proc :: FilePath -> [String] -> Step' CRes-}
    Sh :: String -> Step' a
    {-Templ :: (Data a, Typeable a, Generic a, FromJSON a) => FilePath -> FilePath -> Step' a-}



--XXX useTemplate (Templ src dst) = liftIO $ useTemplate (toTemplate src :: Template a) dst >> return Nothing


run :: Show a => Step' a -> StepM ()
run = runWith id (return . const ())

runWith :: Show a => (CreateProcess -> CreateProcess) -> (Process -> IO b) -> Step' a -> StepM b
runWith with f (Sh cmd) = StepM $ do
    p <- createProcess $ with $ shell cmd
    b <- f p
    return (b, Just p)

runPipeIn :: Show a => Handle -> Step' a -> StepM ()
runPipeIn hin = runWith setIn (return . const ())
  where setIn p = p { std_in = UseHandle hin }

runPipeOut :: Show a => Step' a -> StepM Handle
runPipeOut = runWith setOut takeOut
  where setOut p                  = p { std_out = CreatePipe }
        takeOut (_, Just o, _, _) = return o

runReadOut :: Show a => Step' a -> StepM Text
runReadOut = runWith setOut readOut
  where setOut p                  = p { std_out = CreatePipe }
        readOut (_, Just o, _, _) = T.hGetContents o

(-|-) :: (Show a, Show b) => Step' a -> Step' b -> StepM ()
producer -|- consumer = do
    let (StepM ma) = runPipeOut producer
    (ho, _) <- liftIO $ ma
    runPipeIn ho consumer






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
