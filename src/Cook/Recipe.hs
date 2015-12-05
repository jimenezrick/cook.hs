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

  , defRecipeConf
  , runRecipe

  , F.FsTree (..)
  , F.Content (..)

  , F.defAttrs
  , createFsTree
  ) where

--
-- TODO: withSudo, withSudoUser, check withCd and command that always fails?
--

import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Data.Text.Lazy (Text, empty)
import Network.BSD
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process (CreateProcess (..))
import Text.Printf

import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

import qualified Cook.Recipe.FsTree as F

type Trace = [(Step, FilePath)]

data RecipeConf = RecipeConf {
    recipeConfRoot :: FilePath
  , recipeHostName :: String
  }

data Ctx = Ctx {
    ctxCwd        :: Maybe FilePath
  , ctxTrace      :: Trace
  , ctxRecipeConf :: RecipeConf
  }

type Code = Int

type Recipe = ExceptT (Trace, Code) (StateT Ctx IO)

data Step = Proc FilePath [String]
          | Shell String

instance Show Step where
    show (Proc prog args) = printf "process: %s %s" prog (unwords args)
    show (Shell cmd)      = printf "shell: %s" cmd

defRecipeConf :: IO RecipeConf
defRecipeConf = do
    root <- getCurrentDirectory
    hostname <- getHostName
    return RecipeConf { recipeConfRoot = root , recipeHostName = hostname }

createFsTree :: FilePath -> F.FsTree -> Recipe ()
createFsTree base fstree = liftIO $ F.createFsTree base fstree

proc :: FilePath -> [String] -> Step
proc = Proc

proc' :: FilePath -> Step
proc' prog = Proc prog []

sh :: String -> Step
sh = Shell

withCd :: FilePath -> Recipe a -> Recipe a
withCd dir recipe = do
    ctx <- get
    (t, a) <- hoist (withStateT $ chdir dir) $ do
        a <- recipe
        t <- ctrace
        return (t, a)
    put ctx { ctxTrace = t }
    return a
  where chdir to | isAbsolute to = \ctx -> ctx { ctxCwd = Just to }
                 | otherwise     = \ctx ->
                     case ctxCwd ctx of
                         Nothing -> ctx { ctxCwd = Just to }
                         Just d' -> ctx { ctxCwd = Just $ d' </> to }

cwdir :: Recipe (Maybe FilePath)
cwdir = gets ctxCwd

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
    modify $ \ctx@(Ctx { ctxTrace = t }) -> ctx { ctxTrace = (step, d):t }

ctrace :: Recipe Trace
ctrace = gets ctxTrace

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

runRecipe :: RecipeConf -> Recipe a -> IO ()
runRecipe conf recipe = do
    let ctx = Ctx { ctxCwd = Nothing, ctxTrace = mempty, ctxRecipeConf = conf }
    r <- flip evalStateT ctx $ runExceptT recipe
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
