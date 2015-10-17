module Plan where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Text.Lazy
import Data.Text.Lazy.IO as T
import System.Exit
import System.Process.Text.Lazy

type Plan a = ExceptT (Int, a) (WriterT [Step] IO) a

data Step = Cmd FilePath [String] deriving Show

cmd :: FilePath -> [String] -> Step
cmd cmd' args = Cmd cmd' args

-- TODO: runCapture
run :: Step -> Plan (Text, Text)
run step@(Cmd cmd' args) = do
    tell [step]
    (exit, out, err) <- liftIO $ readProcessWithExitCode cmd' args empty
    case exit of
        ExitSuccess      -> return (out, err)
        ExitFailure code -> throwError (code, (out, err))

main :: IO ()
main = do
    res <- runWriterT $ runExceptT $ do
        (o, _) <- run $ cmd "true" []
        {-(o, _) <- run $ cmd "false" []-}
        liftIO $ T.putStr o
        (o, _) <- run $ cmd "echo" ["hello"]
        liftIO $ T.putStr o
        run $ cmd "echo" ["exit"]
    print res
