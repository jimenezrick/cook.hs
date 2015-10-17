{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE DeriveFunctor      #-}

module Deploy where

import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as T

import Control.Applicative

import Control.Exception
import Control.Monad.IO.Class
import GHC.IO.Handle
import System.Exit

import Text.Printf

import Data.Typeable
import GHC.Generics
import Data.Yaml

import Data.Tree
import System.Exit
import System.Directory
import System.FilePath
import System.Process

import Template

type Process = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

type ProcessBackground a = (ProcessHandle, Step a)

-- TODO: ExceptT in Trace.hs
-- run :: Step a -> StepM (Either code a)
-- FIXME: Don't wait in the bind operator, instead look at the Left value
newtype StepM a = StepM {
    unStepM :: IO (a, Maybe (Process, Step a))
  } deriving Functor

instance Applicative (StepM) where
    pure = liftIO . return
    StepM mf <*> StepM ma = StepM $ do
        (f, _) <- mf
        (a, p) <- ma
        return (f a, f `fmap3` p)
      where fmap3 = fmap . fmap . fmap

instance Monad (StepM) where
    s >>= f = StepM $ runStepM s >>= unStepM . f

instance MonadIO (StepM) where
    liftIO ma = StepM $ ma >>= \a -> return (a, Nothing)

-- TODO: (NOT Either here as well) `a' is OK
runStepM :: StepM a -> IO a
runStepM (StepM ma) = do
    (a, p) <- ma
    case p of
        Just ((_, _, _, h), s) -> do
            exit <- waitForProcess h
            case exit of
                ExitSuccess      -> return a
                ExitFailure code -> throwIO $ ProcessFailure s code
        Nothing -> return a




data ProcessFailure where ProcessFailure :: Step a -> Int -> ProcessFailure deriving Typeable

instance Show ProcessFailure where
    show (ProcessFailure step code) = printf "Error: %s failed with exit code %d" (show step) code

instance Exception ProcessFailure


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
                                                                           -- (Process -> Step a -> IO a)
    Cmd        :: FilePath -> [String] -> (CreateProcess -> CreateProcess) -> (Process -> IO a) -> Step a
    Sh         :: String -> (CreateProcess -> CreateProcess) -> (Process -> IO a) -> Step a
    Pipe       :: Step a -> Step b -> Step b
    Background :: Step a -> Step (ProcessBackground a)
    {- TODO: Templ :: (Data a, Typeable a, Generic a, FromJSON a) => FilePath -> FilePath -> Step a-}

instance Functor Step -- XXX

instance Show (Step a) where
    show (Cmd cmd args _ _) = printf "Cmd %s %s" (show cmd) (show args)
    show (Sh script _ _) = printf "Sh %s" (show script)
    -- TODO: rest

cmd :: FilePath -> [String] -> Step ()
cmd prog args = Cmd prog args id $ return . const ()

cmd_ :: FilePath -> Step ()
cmd_ prog = cmd prog []

sh :: String -> Step ()
sh script = Sh script id $ return . const ()

infixr 0 .|

(.|) :: Step a -> Step b -> Step b
producer .| consumer | Background _ <- producer  = error "(.|): background step are not allowed in pipes"
                     | Background _ <- consumer  = error "(.|): background step are not allowed in pipes"
                     | Pipe _ _     <- producer  = error "(.|): pipe is a right-associative operator"
                     | otherwise                 = Pipe producer consumer

run :: Step a -> StepM a
run step@(Cmd cmd args fproc fa) = StepM $ do
    p <- createProcess $ fproc $ proc cmd args
    a <- fa p
    return (a, Just (p, step))
run step@(Sh script fproc fa) = StepM $ do
    p <- createProcess $ fproc $ shell script
    a <- fa p
    return (a, Just (p, step))
run (Pipe cons prod)  = runPipe cons prod
run (Background step) = runInBackground step

runPipe :: Step a -> Step b -> StepM b
runPipe producer consumer = do
    let (StepM ma) = run $ withOutPipe producer
    (ho, _) <- liftIO ma
    run $ withInHandle ho consumer

runInBackground :: Step a -> StepM (ProcessBackground a)
runInBackground step = do
    let (StepM ma) = run step
    (_, Just ((_, _, _, hp), s)) <- liftIO ma
    return (hp, s)

waitFinished :: ProcessBackground a -> StepM ()
waitFinished (proc, step) = do
    exit <- liftIO $ waitForProcess proc
    case exit of
        ExitSuccess      -> return ()
        ExitFailure code -> liftIO $ throwIO $ ProcessFailure step code

withProc :: (CreateProcess -> CreateProcess) -> Step a -> Step a
withProc fproc (Cmd cmd args fproc' fa) = Cmd cmd args (fproc . fproc') fa
withProc fproc (Sh script fproc' fa)    = Sh script (fproc . fproc') fa
withProc fproc (Pipe cons prod)         = Pipe (withProc fproc cons) prod

withResult :: (Process -> IO b) -> Step a -> Step b
withResult fb (Cmd cmd args fproc _) = Cmd cmd args fproc fb
withResult fb (Sh script fproc _)    = Sh script fproc fb
withResult fb (Pipe cons prod)       = Pipe cons (withResult fb prod)

withInHandle :: Handle -> Step a -> Step a
withInHandle hin = withProc (\p -> p { std_in = UseHandle hin })

withOutPipe :: Step a -> Step Handle
withOutPipe = withProc (\p -> p { std_out = CreatePipe }) . withResult (\(_, Just hout, _, _) -> return hout)

withOutText :: Step a -> Step Text
withOutText = withResult (\(_, Just hout, _, _) -> T.hGetContents hout) . withOutPipe

inBackground :: Step a -> Step (ProcessBackground a)
inBackground (Background _) = error "inBackground: step already marked as background"
inBackground step           = Background step




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
