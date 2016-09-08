module Cook.Recipe (
    Recipe
  , RecipeConf (..)
  , Step

  , proc
  , proc'
  , sh

  , run
  , runProc
  , runProc'
  , runSh
  , runWith
  , runRead
  , runReadWith
  , runTakeRead
  , runTakeReadWith
  , runPipe
  , runPipeRead
  , runPipeTakeRead

  , withRecipeName
  , withCd
  , withEnv
  , withoutError
  , withSudo
  , withSudoUser

  , defRecipeConf
  , recipeConf
  , runRecipe
  , failWith
  , failWith'

  , F.FsTree (..)
  , F.Content (..)

  , F.defAttrs
  , createFsTree

  , getEnv
  ) where

import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Text.Lazy (Text, empty)
import Network.BSD
import System.Directory
import System.Environment (lookupEnv, getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.Process (CreateProcess (..))
import Text.Printf

import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy.IO as T
import qualified System.Process as P
import qualified System.Process.Text.Lazy as PT

import qualified Cook.Recipe.FsTree as F

data RecipeConf = RecipeConf {
    recipeConfDebug    :: Bool
  , recipeConfVerbose  :: Bool
  , recipeTraceLength  :: Int
  , recipeConfRootDir  :: FilePath
  , recipeConfHostName :: String
  } deriving Show

data ExecPrivileges = ExecNormal
                    | ExecSudo
                    | ExecSudoUser String
                    deriving Show

data Ctx = Ctx {
    ctxRecipeNames :: [String]
  , ctxCwd         :: Maybe FilePath
  , ctxEnv         :: Maybe [(String, String)]
  , ctxTrace       :: Trace
  , ctxSudo        :: ExecPrivileges
  , ctxRecipeConf  :: RecipeConf
  } deriving Show

type Trace = [(Step, Ctx)]

type Code = Int

type Recipe = ExceptT (Trace, Code) (StateT Ctx IO)

data Step = Proc FilePath [String]
          | Shell String
          | Failure String

instance Show Step where
    show (Proc prog args) = printf "process: %s %s" prog (unwords args)
    show (Shell cmd)      = printf "shell:   %s" cmd
    show (Failure msg)    = printf "failure: %s" msg

defRecipeConf :: IO RecipeConf
defRecipeConf = do
    root <- getCurrentDirectory
    hostname <- getHostName
    return RecipeConf {
        recipeConfDebug    = False
      , recipeConfVerbose  = True
      , recipeTraceLength  = 5
      , recipeConfRootDir  = root
      , recipeConfHostName = hostname
      }

recipeConf :: Recipe RecipeConf
recipeConf = gets ctxRecipeConf

procEnv :: Recipe (Maybe [(String, String)])
procEnv = do
    env <- gets ctxEnv
    case env of
        Nothing   -> return Nothing
        Just env' -> do
            penv <- liftIO getEnvironment
            return . Just . M.toList . M.fromList $ (penv ++ env')

createFsTree :: FilePath -> F.FsTree -> Recipe ()
createFsTree base fstree = liftIO $ F.createFsTree base fstree -- TODO: Catch IO errors

proc :: FilePath -> [String] -> Step
proc = Proc

proc' :: FilePath -> Step
proc' prog = Proc prog []

sh :: String -> Step
sh = Shell

failWith :: String -> Recipe a
failWith msg = do
    trace $ Failure msg
    failWith' 1

failWith' :: Code -> Recipe a
failWith' code = do
    trc <- gets ctxTrace
    throwError (trc, code)

withCtx :: (Ctx -> Ctx) -> Recipe a -> Recipe a
withCtx f recipe = do
    ctx <- get
    (trc, a) <- hoist (withStateT f) $ do
        ctx' <- get
        let conf = ctxRecipeConf ctx'
        when (recipeConfDebug conf) $
            liftIO $ hPrintf stderr "Cook: using %s\n" (show ctx')
        a <- recipe
        trc <- gets ctxTrace
        return (trc, a)
    put ctx { ctxTrace = trc }
    return a

withRecipeName :: String -> Recipe a -> Recipe a
withRecipeName name recipe = withCtx (\ctx@Ctx {..} -> ctx { ctxRecipeNames = name:ctxRecipeNames }) $ do
    ctx <- get
    let conf = ctxRecipeConf ctx
    case showCtxRecipeName ctx of
        Just recipeName | recipeConfVerbose conf -> liftIO $ hPrintf stderr "Cook: in %s\n" recipeName
        _                                        -> return ()
    recipe

withCd :: FilePath -> Recipe a -> Recipe a
withCd dir = withCtx (chdir dir)
  where chdir to | isAbsolute to = \ctx -> ctx { ctxCwd = Just to }
                 | otherwise     = \ctx ->
                     case ctxCwd ctx of
                         Nothing -> ctx { ctxCwd = Just to }
                         Just d' -> ctx { ctxCwd = Just $ d' </> to }

withEnv :: [(String, String)] -> Recipe a -> Recipe a
withEnv env = withCtx (\ctx -> ctx { ctxEnv = Just env })

withoutError :: Recipe a -> Recipe (Either Code a)
withoutError recipe = (Right <$> recipe) `catchError` \(_, code) -> return $ Left code

withSudo :: Recipe a -> Recipe a
withSudo = withCtx $ \ctx -> ctx { ctxSudo = ExecSudo }

withSudoUser :: String -> Recipe a -> Recipe a
withSudoUser user | null user = error "Recipe.withSudoUser: invalid user"
                  | otherwise = withCtx $ \ctx -> ctx { ctxSudo = ExecSudoUser user }

absoluteCwd :: Recipe FilePath
absoluteCwd = do
    d <- gets ctxCwd
    case d of
        Nothing                   -> liftIO getCurrentDirectory
        Just dir | isAbsolute dir -> return dir
                 | otherwise      -> liftIO $ canonicalizePath dir

trace :: Step -> Recipe ()
trace step = do
    acwd <- absoluteCwd
    modify $ \ctx@Ctx {..} -> ctx { ctxTrace = (step, ctx { ctxCwd = Just acwd }):ctxTrace }

buildProcProg :: ExecPrivileges -> FilePath -> [String] -> (FilePath, [String])
buildProcProg ExecNormal          prog args = (prog, args)
buildProcProg ExecSudo            prog args = ("sudo", prog:args)
buildProcProg (ExecSudoUser user) prog args = ("sudo", ["-u", user] ++ prog:args)

buildShellCmd :: ExecPrivileges -> String -> String
buildShellCmd sudoMode = case sudoMode of
                             ExecNormal        -> strictMode
                             ExecSudo          -> strictMode . sudo
                             ExecSudoUser user -> strictMode . sudoUser user
  where strictMode = ("set -o errexit -o nounset -o pipefail; " ++)
        sudo       = ("sudo " ++)
        sudoUser u = (("sudo -u " ++ u ++ " " :: String) ++)

run :: Step -> Recipe ()
run step = do
    sudo <- gets ctxSudo
    trace step
    case step of
        Proc prog args -> runWith $ uncurry P.proc $ buildProcProg sudo prog args
        Shell cmd      -> runWith $ P.shell $ buildShellCmd sudo cmd
        Failure _      -> error "Recipe.run: unreachable case"

runProc :: FilePath -> [String] -> Recipe ()
runProc = (fmap . fmap) run proc

runProc' :: FilePath -> Recipe ()
runProc' = run . proc'

runSh :: String -> Recipe ()
runSh = run . sh

runWith :: CreateProcess -> Recipe ()
runWith p = do
    dir <- gets ctxCwd
    env <- procEnv
    (_, _, _, ph) <- liftIO $ P.createProcess p { cwd = dir, env = env }
    exit <- liftIO $ P.waitForProcess ph
    case exit of
        ExitFailure code -> do
            trc <- gets ctxTrace
            throwError (trc, code)
        _ -> return ()

runRead :: Step -> Recipe (Text, Text)
runRead step = runTakeRead step empty

runReadWith :: CreateProcess -> Recipe (Text, Text)
runReadWith p = runTakeReadWith p empty

runTakeRead :: Step -> Text -> Recipe (Text, Text)
runTakeRead step input = do
    sudo <- gets ctxSudo
    trace step
    case step of
        Proc prog args -> runTakeReadWith (uncurry P.proc $ buildProcProg sudo prog args) input
        Shell cmd      -> runTakeReadWith (P.shell $ buildShellCmd sudo cmd) input
        Failure _      -> error "Recipe.runTakeRead: unreachable case"

runTakeReadWith :: CreateProcess -> Text -> Recipe (Text, Text)
runTakeReadWith p input = do
    dir <- gets ctxCwd
    env <- procEnv
    (exit, out, err) <- liftIO $ PT.readCreateProcessWithExitCode p { cwd = dir, env = env } input
    case exit of
        ExitFailure code -> do
            trc <- gets ctxTrace
            throwError (trc, code)
        _ -> return (out, err)

runRecipe :: RecipeConf -> Recipe a -> IO ()
runRecipe conf recipe = do
    let ctx = Ctx {
        ctxRecipeNames = []
      , ctxCwd         = Nothing
      , ctxEnv         = Nothing
      , ctxTrace       = mempty
      , ctxSudo        = ExecNormal
      , ctxRecipeConf  = conf
      }
    when (recipeConfVerbose conf) $
        hPrintf stderr "Cook: running with %s\n" (show conf)
    r <- flip evalStateT ctx $ runExceptT recipe
    case r of
        Left ([], _)                   -> error "Recipe.runRecipe: empty trace"
        Left (trc@((_, ctx'):_), code) -> do
            printTrace (recipeTraceLength conf) trc
            hPrintf stderr "Cook: process exited with code %d\n" code
            hPrintf stderr "      using %s\n" (show ctx' { ctxTrace = [] })
        Right _ | recipeConfVerbose conf -> hPutStrLn stderr "Cook: recipe successful"
                | otherwise              -> return ()

runPipe :: [Step] -> Recipe ()
runPipe pipe = runPipeTakeRead pipe empty >>= liftIO . T.putStr

runPipeRead :: [Step] -> Recipe Text
runPipeRead pipe = runPipeTakeRead pipe empty

runPipeTakeRead :: [Step] -> Text -> Recipe Text
runPipeTakeRead []     input = return input
runPipeTakeRead (s:ss) input = do
    (out, _) <- runTakeRead s input
    runPipeTakeRead ss out

printTrace :: Int -> Trace -> IO ()
printTrace _ []                     = return ()
printTrace n (failed@(_, ctx):prev) = do
    let name = maybe "" (" " ++) (showCtxRecipeName ctx)
    hPrintf stderr "Cook: error in recipe%s:\n" name
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take n prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, ctx') = let names = maybe "" (++ " ") (showCtxRecipeName ctx')
                           in printf "%s (%sfrom %s)" (show step) names (fromJust $ ctxCwd ctx')

showCtxRecipeName :: Ctx -> Maybe String
showCtxRecipeName Ctx { ctxRecipeNames = [] } = Nothing
showCtxRecipeName Ctx { ctxRecipeNames = ns } = Just $ intercalate "." $ reverse ns

getEnv :: String -> Recipe String
getEnv var = do
    val <- liftIO $ lookupEnv var
    maybe (failWith $ printf "getEnv: %s is not defined" var) return val
