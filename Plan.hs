module Plan (
    proc
  , proc'
  , sh

  , withCd

  , run
  , runWith
  , runRead
  , runReadWith

  , runPlan
  ) where

import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Data.Text.Lazy (Text, empty)
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process (CreateProcess (..))
import Text.Printf

import qualified Data.Text.Lazy.IO as T
import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

type Cwd = Maybe FilePath

type Trace = [(Step, FilePath)]

type Ctx = (Cwd, Trace)

type Code = Int

type Plan = ExceptT (Trace, Code) (StateT Ctx IO)

--
-- TODO: data Pipe = Pipe Step Pipe
--

data Step = Proc FilePath [String]
          | Shell String

instance Show Step where
    show (Proc prog args) = printf "process: %s %s" prog (unwords args)
    show (Shell cmd)      = printf "shell: %s" cmd

proc :: FilePath -> [String] -> Step
proc = Proc

proc' :: FilePath -> Step
proc' prog = Proc prog []

sh :: String -> Step
sh = Shell

withCd :: FilePath -> Plan a -> Plan a
withCd dir plan = do
    d <- cwdir
    (t, a) <- hoist (withStateT $ chdir dir) $ do
        a <- plan
        t <- ctrace
        return (t, a)
    put (d, t)
    return a
  where chdir to | isAbsolute to = \(_, t) -> (Just to, t)
                 | otherwise     = \(d, t) ->
                     case d of
                         Nothing -> (Just to, t)
                         Just d' -> (Just $ d' </> to, t)

cwdir :: Plan (Maybe FilePath)
cwdir = gets fst

absoluteCwd :: Plan FilePath
absoluteCwd = do
    d <- cwdir
    case d of
        Nothing                   -> liftIO getCurrentDirectory
        Just dir | isAbsolute dir -> return dir
                 | otherwise      -> liftIO $ canonicalizePath dir

trace :: Step -> Plan ()
trace step = do
    d <- absoluteCwd
    modify $ fmap ((step, d):)

ctrace :: Plan Trace
ctrace = gets snd

run :: Step -> Plan ()
run step@(Proc prog args) = trace step >> runWith (P.proc prog args)
run step@(Shell cmd)      = trace step >> runWith (P.shell cmd)

runWith :: CreateProcess -> Plan ()
runWith p = do
    dir <- cwdir
    (_, _, _, ph) <- liftIO $ P.createProcess p { cwd = dir }
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitSuccess   -> return ()
        ExitFailure c -> do
            t <- ctrace
            throwError (t, c)

runRead :: Step -> Plan (Text, Text)
runRead step@(Proc prog args) = trace step >> runReadWith (P.proc prog args)
runRead step@(Shell cmd)      = trace step >> runReadWith (P.shell cmd)

runReadWith :: CreateProcess -> Plan (Text, Text)
runReadWith p = do
    dir <- cwdir
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode p {cwd = dir } empty
    case exit of
        ExitSuccess   -> return (out, err)
        ExitFailure c -> do
            t <- ctrace
            throwError (t, c)

runPlan :: Plan a -> IO ()
runPlan plan = do
    r <- flip evalStateT (Nothing, mempty) $ runExceptT plan
    case r of
        Left (t, c) -> do
            printTrace t
            printf "Process exited with code %d\n" c
        Right _ -> hPutStrLn stderr "Plan successful"

printTrace :: Trace -> IO ()
printTrace (failed:prev) = do
    hPutStrLn stderr "Error executing plan:"
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take 10 prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, trc) = printf "%s (from %s)" (show step) trc
printTrace [] = error "Plan.printTrace: empty trace"

--------------------------------------------------------------------------------
main :: IO ()
main = runPlan $ do
    (o, _) <- foo
    liftIO $ print o
    run $ sh "pwd"

    withCd ".." $ do
        run $ sh "pwd"
        (o2, _) <- runRead $ sh "false"
        liftIO $ T.putStr o2

    (o3, _) <- runRead $ proc "echo" ["hello"]
    liftIO $ T.putStr o3
    runRead $ proc "echo" ["exit"]

foo :: Plan (Text, Text)
foo = withCd "/" $ do
    run $ proc' "pwd"
    run $ proc' "true"
    runRead $ sh "echo xxx"
