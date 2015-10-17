module Plan where

import Control.Monad.Except
import Control.Monad.State
import Data.Text.Lazy (Text, empty)

import System.Exit
import System.IO
import System.Process (CreateProcess)
import Text.Printf

import qualified Data.Text.Lazy.IO as T
import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

type Trace = [Step]

type Cwd = Maybe FilePath

type Ctx = (Cwd, Trace)

type Code = Int

type Plan a = ExceptT (Trace, Code, a) (StateT Ctx IO) a

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

trace :: Step -> StateT Ctx IO ()
trace step = modify $ fmap $ (step:)

run :: Step -> Plan ()
run step@(Proc prog args) = runWith step $ P.proc prog args
run step@(Shell cmd)      = runWith step $ P.shell cmd

runWith :: Step -> CreateProcess -> Plan ()
runWith step proc = do
    lift $ trace step
    (_, _, _, ph) <- liftIO $ P.createProcess proc
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitSuccess      -> return ()
        ExitFailure code -> do
            t <- snd <$> get
            throwError (t, code, ())

runRead :: Step -> Plan (Text, Text)
runRead step@(Proc prog args) = runReadWith step $ P.proc prog args
runRead step@(Shell cmd)      = runReadWith step $ P.shell cmd

runReadWith :: Step -> CreateProcess -> Plan (Text, Text)
runReadWith step proc = do
    lift $ trace step
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode proc empty
    case exit of
        ExitSuccess      -> return (out, err)
        ExitFailure code -> do
            t <- snd <$> get
            throwError (t, code, (out, err))

runPlan :: Plan a -> IO ()
runPlan plan = do
    r <- flip evalStateT (Nothing, mempty) $ runExceptT plan
    case r of
        Left (trace, code, a) -> do
            printTrace trace
            printf "Process exited with code %d\n" code

printTrace :: Trace -> IO ()
printTrace (err:normal) = do
    putLine
    hPutStrLn stderr "Error executing plan:"
    putLine
    mapM_ (hPutStrLn stderr . ("   " ++) . show) $ reverse $ take 10 normal
    hPutStrLn stderr $ "-> " ++ show err
  where putLine = hPutStrLn stderr $ replicate 70 '-'

main :: IO ()
main = do
    runPlan $ do
        (o2, _) <- foo
        liftIO $ print o2

        (o, _) <- runRead $ sh "false"
        {-(o, _) <- runRead $ cmd "false" []-}
        liftIO $ T.putStr o
        (o, _) <- runRead $ proc "echo" ["hello"]
        liftIO $ T.putStr o
        runRead $ proc "echo" ["exit"]

foo :: Plan (Text, Text)
foo = do
    runRead $ proc "true" []
    runRead $ proc "echo" ["xxx"]

-- TODO:
-- cd $ do
--    something
