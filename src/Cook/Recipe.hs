module Cook.Recipe (
    Recipe
  , Step

  , proc
  , proc'
  , sh

  , withCd

  , run
  , runWith
  , runRead
  , runReadWith

  , runRecipe
  ) where

--
-- TODO: withSudo, withSudoUser
--

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

import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

type Cwd = Maybe FilePath

type Trace = [(Step, FilePath)]

type Ctx = (Cwd, Trace)

type Code = Int

type Recipe = ExceptT (Trace, Code) (StateT Ctx IO)

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

withCd :: FilePath -> Recipe a -> Recipe a
withCd dir recipe = do
    d <- cwdir
    (t, a) <- hoist (withStateT $ chdir dir) $ do
        a <- recipe
        t <- ctrace
        return (t, a)
    put (d, t)
    return a
  where chdir to | isAbsolute to = \(_, t) -> (Just to, t)
                 | otherwise     = \(d, t) ->
                     case d of
                         Nothing -> (Just to, t)
                         Just d' -> (Just $ d' </> to, t)

cwdir :: Recipe (Maybe FilePath)
cwdir = gets fst

absoluteCwd :: Recipe FilePath
absoluteCwd = do
    d <- cwdir
    case d of
        Nothing                   -> liftIO getCurrentDirectory
        Just dir | isAbsolute dir -> return dir
                 | otherwise      -> liftIO $ canonicalizePath dir

trace :: Step -> Recipe ()
trace step = do
    d <- absoluteCwd
    modify $ fmap ((step, d):)

ctrace :: Recipe Trace
ctrace = gets snd

buildCmd :: String -> String
buildCmd cmd = "set -o errexit -o nounset -o pipefail;" ++ cmd

run :: Step -> Recipe ()
run step@(Proc prog args) = trace step >> runWith (P.proc prog args)
run step@(Shell cmd)      = trace step >> runWith (P.shell $ buildCmd cmd)

runWith :: CreateProcess -> Recipe ()
runWith p = do
    dir <- cwdir
    (_, _, _, ph) <- liftIO $ P.createProcess p { cwd = dir }
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitSuccess   -> return ()
        ExitFailure c -> do
            t <- ctrace
            throwError (t, c)

runRead :: Step -> Recipe (Text, Text)
runRead step@(Proc prog args) = trace step >> runReadWith (P.proc prog args)
runRead step@(Shell cmd)      = trace step >> runReadWith (P.shell $ buildCmd cmd)

runReadWith :: CreateProcess -> Recipe (Text, Text)
runReadWith p = do
    dir <- cwdir
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode p {cwd = dir } empty
    case exit of
        ExitSuccess   -> return (out, err)
        ExitFailure c -> do
            t <- ctrace
            throwError (t, c)

runRecipe :: Recipe a -> IO ()
runRecipe recipe = do
    r <- flip evalStateT (Nothing, mempty) $ runExceptT recipe
    case r of
        Left (t, c) -> do
            printTrace t
            printf "Process exited with code %d\n" c
        Right _ -> hPutStrLn stderr "Recipe successful"

printTrace :: Trace -> IO ()
printTrace (failed:prev) = do
    hPutStrLn stderr "Error in recipe:"
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take 10 prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, trc) = printf "%s (from %s)" (show step) trc
printTrace [] = error "Recipe.printTrace: empty trace"
