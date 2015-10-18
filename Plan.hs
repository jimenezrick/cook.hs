module Plan where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Morph
import Data.Text.Lazy (Text, empty)

import System.Exit
import System.FilePath
import System.IO
import System.Process (CreateProcess (..))
import Text.Printf

import qualified Data.Text.Lazy.IO as T
import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

type Trace = [Step]

type Cwd = Maybe FilePath

type Ctx = (Cwd, Trace)

type Code = Int

type Plan = ExceptT (Trace, Code) (StateT Ctx IO)

data Step = Proc FilePath [String]
          | Shell String
          -- TODO: Pipe (in a different data type)

instance Show Step where
    show (Proc prog args) = printf "Proc %s %s" prog (unwords args)
    show (Shell cmd) = printf "Proc %s" cmd

proc :: FilePath -> [String] -> Step
proc prog args = Proc prog args

sh :: String -> Step
sh cmd = Shell cmd

withCd :: FilePath -> Plan a -> Plan a
withCd dir plan = do
    cwd <- gets fst
    (trace, a) <- hoist (withStateT $ chdir dir) $ do
        a <- plan
        trace <- gets snd
        return (trace, a)
    put (cwd, trace)
    return a
  where chdir dir | isAbsolute dir = \(_, trace) -> (Just dir, trace)
                  | isRelative dir = \(cwd, trace) -> ((</> dir) <$> cwd, trace)

cwdir :: Plan (Maybe FilePath)
cwdir = gets fst

trace :: Step -> Plan ()
trace step = modify $ fmap (step:)

run :: Step -> Plan ()
run step@(Proc prog args) = runWith step $ P.proc prog args
run step@(Shell cmd)      = runWith step $ P.shell cmd

runWith :: Step -> CreateProcess -> Plan ()
runWith step proc = do
    trace step
    dir <- cwdir
    (_, _, _, ph) <- liftIO $ P.createProcess proc { cwd = dir }
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitSuccess      -> return ()
        ExitFailure code -> do
            t <- snd <$> get
            throwError (t, code)

runRead :: Step -> Plan (Text, Text)
runRead step@(Proc prog args) = runReadWith step $ P.proc prog args
runRead step@(Shell cmd)      = runReadWith step $ P.shell cmd

runReadWith :: Step -> CreateProcess -> Plan (Text, Text)
runReadWith step proc = do
    trace step
    dir <- cwdir
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode proc {cwd = dir } empty
    case exit of
        ExitSuccess      -> return (out, err)
        ExitFailure code -> do
            t <- snd <$> get
            throwError (t, code)

runPlan :: Plan a -> IO ()
runPlan plan = do
    r <- flip evalStateT (Nothing, mempty) $ runExceptT plan
    case r of
        Left (trace, code) -> do
            printTrace trace
            printf "Process exited with code %d\n" code -- TODO: Show CWD
        Right _ -> hPutStrLn stderr "Plan successful"

printTrace :: Trace -> IO ()
printTrace (err:normal) = do
    hPutStrLn stderr "Error executing plan - trace:"
    mapM_ (hPutStrLn stderr . ("  " ++) . show) $ reverse $ take 10 normal
    hPutStrLn stderr $ "> " ++ show err

main :: IO ()
main = do
    runPlan $ do
        (o2, _) <- foo
        liftIO $ print o2
        run $ sh "pwd"

        (o, _) <- runRead $ sh "false"
        {-(o, _) <- runRead $ cmd "false" []-}
        liftIO $ T.putStr o
        (o, _) <- runRead $ proc "echo" ["hello"]
        liftIO $ T.putStr o
        runRead $ proc "echo" ["exit"]

foo :: Plan (Text, Text)
foo = do
    withCd "/" $ do
        run $ sh "pwd"
        runRead $ proc "true" []
        runRead $ proc "echo" ["xxx"]
