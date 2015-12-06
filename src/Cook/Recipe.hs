module Cook.Recipe (
    Recipe
  , RecipeConf (..)
  , Step

  , proc
  , proc'
  , sh

  , run
  , runWith
  , runRead
  , runReadWith

  , withCd
  , withoutError
  , withSudo
  , withSudoUser

  , defRecipeConf
  , recipeConf
  , runRecipe

  , F.FsTree (..)
  , F.Content (..)

  , F.defAttrs
  , createFsTree
  ) where

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
    recipeConfName     :: Maybe String
  , recipeConfVerbose  :: Bool
  , recipeConfRootDir  :: FilePath
  , recipeConfHostName :: String
  } deriving Show

data Ctx = Ctx {
    ctxCwd         :: Maybe FilePath
  , ctxTrace       :: Trace
  , ctxIgnoreError :: Bool
  , ctxSudo        :: Maybe (Maybe String)
  , ctxRecipeConf  :: RecipeConf
  }

type Code = Int

-- TODO: Have a Recipe name (hierarchical)?
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
    return RecipeConf {
        recipeConfName     = Nothing
      , recipeConfVerbose  = True
      , recipeConfRootDir  = root
      , recipeConfHostName = hostname
      }

recipeConf :: Recipe RecipeConf
recipeConf = gets ctxRecipeConf

createFsTree :: FilePath -> F.FsTree -> Recipe ()
createFsTree base fstree = liftIO $ F.createFsTree base fstree

proc :: FilePath -> [String] -> Step
proc = Proc

proc' :: FilePath -> Step
proc' prog = Proc prog []

sh :: String -> Step
sh = Shell

withCtx :: (Ctx -> Ctx) -> Recipe a -> Recipe a
withCtx f recipe = do
    ctx <- get
    (t, a) <- hoist (withStateT f) $ do
        a <- recipe
        t <- gets ctxTrace
        return (t, a)
    put ctx { ctxTrace = t }
    return a

withCd :: FilePath -> Recipe a -> Recipe a
withCd dir = withCtx (chdir dir)
  where chdir to | isAbsolute to = \ctx -> ctx { ctxCwd = Just to }
                 | otherwise     = \ctx ->
                     case ctxCwd ctx of
                         Nothing -> ctx { ctxCwd = Just to }
                         Just d' -> ctx { ctxCwd = Just $ d' </> to }

withoutError :: Recipe a -> Recipe a
withoutError = withCtx $ \ctx -> ctx { ctxIgnoreError = True }

withSudo :: Recipe a -> Recipe a
withSudo = withCtx $ \ctx -> ctx { ctxSudo = Just Nothing }

withSudoUser :: String -> Recipe a -> Recipe a
withSudoUser user | null user = error "Recipe.withSudoUser: invalid user"
                  | otherwise = withCtx $ \ctx -> ctx { ctxSudo = Just (Just user) }

absoluteCwd :: Recipe FilePath
absoluteCwd = do
    d <- gets ctxCwd
    case d of
        Nothing                   -> liftIO getCurrentDirectory
        Just dir | isAbsolute dir -> return dir
                 | otherwise      -> liftIO $ canonicalizePath dir

trace :: Step -> Recipe ()
trace step = do
    d <- absoluteCwd
    modify $ \ctx@(Ctx { ctxTrace = t }) -> ctx { ctxTrace = (step, d):t }

buildProcProg :: Maybe (Maybe String) -> FilePath -> [String] -> (FilePath, [String])
buildProcProg Nothing            prog args = (prog, args)
buildProcProg (Just Nothing)     prog args = ("sudo", prog:args)
buildProcProg (Just (Just user)) prog args = ("sudo", ["-u", user] ++ prog:args)

buildShellCmd :: Maybe (Maybe String) -> String -> String
buildShellCmd sudoMode = case sudoMode of
                             Nothing          -> strictMode
                             Just Nothing     -> strictMode . sudo
                             Just (Just user) -> strictMode . sudoUser user
  where strictMode = ("set -o errexit -o nounset -o pipefail; " ++)
        sudo       = ("sudo " ++)
        sudoUser u = (("sudo -u " ++ u ++ " " :: String) ++)

run :: Step -> Recipe ()
run step = do
    sudo <- gets ctxSudo
    trace step
    runWith $ case step of
                  Proc prog args -> uncurry P.proc $ buildProcProg sudo prog args
                  Shell cmd      -> P.shell $ buildShellCmd sudo cmd

runWith :: CreateProcess -> Recipe ()
runWith p = do
    dir <- gets ctxCwd
    noErr <- gets ctxIgnoreError
    (_, _, _, ph) <- liftIO $ P.createProcess p { cwd = dir }
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitFailure c | not noErr -> do
            t <- gets ctxTrace
            throwError (t, c)
        _ -> return ()

runRead :: Step -> Recipe (Text, Text)
runRead step = do
    sudo <- gets ctxSudo
    trace step
    runReadWith $ case step of
                      Proc prog args -> uncurry P.proc $ buildProcProg sudo prog args
                      Shell cmd      -> P.shell $ buildShellCmd sudo cmd

runReadWith :: CreateProcess -> Recipe (Text, Text)
runReadWith p = do
    dir <- gets ctxCwd
    noErr <- gets ctxIgnoreError
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode p {cwd = dir } empty
    case exit of
        ExitFailure c | not noErr -> do
            t <- gets ctxTrace
            throwError (t, c)
        _ -> return (out, err)

runRecipe :: RecipeConf -> Recipe a -> IO ()
runRecipe conf recipe = do
    let ctx = Ctx {
        ctxCwd         = Nothing
      , ctxTrace       = mempty
      , ctxIgnoreError = False
      , ctxSudo        = Nothing
      , ctxRecipeConf  = conf
      }
    hPrintf stderr "Cook: running with %s\n" (show conf)
    r <- flip evalStateT ctx $ runExceptT recipe
    case r of
        Left (t, c) -> do
            printTrace t
            hPrintf stderr "Cook: process exited with code %d\n" c
        Right _ | recipeConfVerbose conf -> hPutStrLn stderr "Cook: recipe successful"
                | otherwise              -> return ()

printTrace :: Trace -> IO ()
printTrace (failed:prev) = do
    hPutStrLn stderr "Cook: error in recipe:"
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take 10 prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, trc) = printf "%s (from %s)" (show step) trc
printTrace [] = error "Recipe.printTrace: empty trace"
