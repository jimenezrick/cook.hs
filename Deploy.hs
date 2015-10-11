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

{-data Owner = KeepOwner -- | ...-}

{-data Mode = KeepMode -- | ...-}

--XXX useTemplate (Templ src dst) = liftIO $ useTemplate (toTemplate src :: Template a) dst >> return Nothing

data Step a where
    Cmd  :: FilePath -> [String] -> (CreateProcess -> CreateProcess) -> (Process -> IO a) -> Step a
    Sh   :: String -> (CreateProcess -> CreateProcess) -> (Process -> IO a) -> Step a
    Pipe :: Step a -> Step b -> Step b
    {-Templ :: (Data a, Typeable a, Generic a, FromJSON a) => FilePath -> FilePath -> Step a-}

cmd :: FilePath -> [String] -> Step ()
cmd prog args = Cmd prog args id $ return . const ()

cmd_ :: FilePath -> Step ()
cmd_ prog = cmd prog []

sh :: String -> Step ()
sh script = Sh script id $ return . const ()

infixr 0 -|-

(-|-) :: Step a -> Step b -> Step b
producer -|- consumer = Pipe producer consumer

runPipe :: Step a -> Step b -> StepM b
runPipe producer@(Sh _ _ _) consumer = do
    let (StepM ma) = run $ withOutPipe producer
    (ho, _) <- liftIO $ ma
    case consumer of
        Pipe step1 step2 -> run $ Pipe (withInHandle ho step1) step2
        _                -> run $ withInHandle ho consumer

run :: Step a -> StepM a
run (Cmd cmd args fproc fa) = StepM $ do
    p <- createProcess $ fproc $ proc cmd args
    a <- fa p
    return (a, Just p)
run (Sh script fproc fa) = StepM $ do
    p <- createProcess $ fproc $ shell script
    a <- fa p
    return (a, Just p)
run (Pipe cons prod) = runPipe cons prod

withProc :: (CreateProcess -> CreateProcess) -> Step a -> Step a
withProc fproc (Sh script fproc' fa) = Sh script (fproc . fproc') fa

withResult :: (Process -> IO b) -> Step a -> Step b
withResult fb (Sh script fproc _) = Sh script fproc fb

withInHandle :: Handle -> Step a -> Step a
withInHandle hin = withProc (\p -> p { std_in = UseHandle hin })

withOutPipe :: Step a -> Step Handle
withOutPipe = withProc (\p -> p { std_out = CreatePipe }) . withResult (\(_, Just o, _, _) -> return o)

withOutText :: Step a -> Step Text
withOutText = withResult (\(_, Just o, _, _) -> T.hGetContents o) . withOutPipe




-- TODO: implement some kind of nested commands to implement high-level actions

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
 -
 -
 -resolveSrcPath :: FilePath -> FilePath
 -resolveSrcPath path | isAbsolute path = path
 -                    | otherwise       = "files" </> path
 -}
