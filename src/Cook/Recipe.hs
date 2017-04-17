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
  , runRead
  , runRead'
  , runReadWith
  , runTakeRead
  , runTakeRead'
  , runTakeReadWith
  , runPipe
  , runPipeRead
  , runPipeRead'
  , runPipeTakeRead
  , runPipeTakeRead'

  , withRecipeName
  , withCd
  , withEnv
  , withoutError
  , withoutErrorWhen
  , withSudo
  , withSudoUser

  , defRecipeConf
  , recipeConf
  , runRecipe
  , runRecipeConf
  , failWith
  , catchException

  , F.FsTree (..)
  , F.Content (..)

  , F.defAttrs
  , createFsTree

  , getEnv
  , getCwd
  ) where

import Control.Exception.Lifted
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Data.ByteString.Lazy (ByteString, empty)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text.Lazy (Text)
import Network.BSD
import System.Directory
import System.Environment (lookupEnv, getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import System.Process (CreateProcess (..))
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Process as P
import qualified System.Process.ByteString.Lazy as PB

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
  , ctxTrace       :: [Trace]
  , ctxSudo        :: ExecPrivileges
  , ctxRecipeConf  :: RecipeConf
  } deriving Show

type Trace = (Result, Ctx)

type Recipe = ExceptT (NonEmpty Trace) (StateT Ctx IO)

-- XXX: pipe builder
-- XXX: type Input = Maybe Text
data Step = Proc FilePath [String]
          {-| ProcIn FilePath [String] Text-}
          | Shell String
          {-| ShellIn String Text-}

data Result = StepSucceeded Step
            | StepFailed Step Int
            | Failure String
            deriving Show

instance Show Step where
    show (Proc prog args) = printf "process: %s %s" prog (unwords args)
    show (Shell cmd)      = printf "shell:   %s" cmd

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
createFsTree base fstree = catchException . liftIO $ F.createFsTree base fstree

proc :: FilePath -> [String] -> Step
proc = Proc

proc' :: FilePath -> Step
proc' prog = Proc prog []

sh :: String -> Step
sh = Shell

failWith :: String -> Recipe a
failWith msg = do
    traceResult $ Failure msg
    abortRecipe

abortRecipe :: Recipe a
abortRecipe = gets ctxTrace >>= throwError . NE.fromList

catchException :: Recipe a -> Recipe a
catchException = handle (\e -> failWith $ show (e :: SomeException))

withCtx :: (Ctx -> Ctx) -> Recipe a -> Recipe a
withCtx f recipe = do
    ctx <- get
    (trace, a) <- hoist (withStateT f) $ do
        ctx' <- get
        let conf = ctxRecipeConf ctx'
        when (recipeConfDebug conf) $
            liftIO $ hPrintf stderr "Cook: using %s\n" (show ctx')
        a <- recipe
        trace <- gets ctxTrace
        return (trace, a)
    put ctx { ctxTrace = trace }
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
                         Nothing  -> ctx { ctxCwd = Just to }
                         Just cur -> ctx { ctxCwd = Just $ cur </> to }

withEnv :: [(String, String)] -> Recipe a -> Recipe a
withEnv env = withCtx (\ctx -> ctx { ctxEnv = Just env })

withoutError :: Recipe a -> Recipe (Either Result a)
withoutError recipe = (Right <$> recipe) `catchError` \(trace) -> return . Left . fst $ NE.head trace

withoutErrorWhen :: (Int -> Maybe a) -> Recipe a -> Recipe a
withoutErrorWhen isCode recipe = do
    result <- withoutError recipe
    case result of
        Left (StepSucceeded _ )                          -> error "Recipe.withoutErrorWhen: invalid case"
        Left (StepFailed _ code) | Just v <- isCode code -> return v
                                 | otherwise             -> abortRecipe
        Left (Failure _)                                 -> abortRecipe
        Right v                                          -> return v

withSudo :: Recipe a -> Recipe a
withSudo = withCtx $ \ctx -> ctx { ctxSudo = ExecSudo }

withSudoUser :: String -> Recipe a -> Recipe a
withSudoUser user | null user = error "Recipe.withSudoUser: invalid user"
                  | otherwise = withCtx $ \ctx -> ctx { ctxSudo = ExecSudoUser user }

getCwd :: Recipe FilePath
getCwd = do
    d <- gets ctxCwd
    case d of
        Nothing  -> liftIO getCurrentDirectory
        Just dir -> liftIO $ canonicalizePath dir

traceResult :: Result -> Recipe ()
traceResult result = do
    dir <- getCwd
    modify $ \ctx@Ctx {..} -> ctx { ctxTrace = (result, ctx { ctxCwd = Just dir }):ctxTrace }

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
    let proc = case step of
                   Proc prog args -> uncurry P.proc $ buildProcProg sudo prog args
                   Shell cmd      -> P.shell $ buildShellCmd sudo cmd
    dir <- gets ctxCwd
    env <- procEnv
    (_, _, _, p) <- liftIO $ P.createProcess proc { cwd = dir, env = env }
    exit <- liftIO $ P.waitForProcess p
    case exit of
        ExitFailure code -> traceResult (StepFailed step code) >> abortRecipe
        ExitSuccess      -> traceResult $ StepSucceeded step

runProc :: FilePath -> [String] -> Recipe ()
runProc = (fmap . fmap) run proc

runProc' :: FilePath -> Recipe ()
runProc' = run . proc'

runSh :: String -> Recipe ()
runSh = run . sh

runRead :: Step -> Recipe (Text, Text)
runRead step = runTakeRead step T.empty

runRead' :: Step -> Recipe (ByteString, ByteString)
runRead' step = runTakeRead' step empty

runReadWith :: CreateProcess -> Recipe (ByteString, ByteString)
runReadWith p = runTakeReadWith p empty

runTakeRead :: Step -> Text -> Recipe (Text, Text)
runTakeRead step input = do
    (out, err) <- runTakeRead' step $ T.encodeUtf8 input
    return (T.decodeUtf8 out, T.decodeUtf8 err)

runTakeRead' :: Step -> ByteString -> Recipe (ByteString, ByteString)
runTakeRead' step input = do
    undefined
    {-
     -sudo <- gets ctxSudo
     -traceResult step
     -case step of
     -    Proc prog args -> runTakeReadWith (uncurry P.proc $ buildProcProg sudo prog args) input
     -    Shell cmd      -> runTakeReadWith (P.shell $ buildShellCmd sudo cmd) input
     -}

runTakeReadWith :: CreateProcess -> ByteString -> Recipe (ByteString, ByteString)
runTakeReadWith p input = do
    undefined
    {-
     -dir <- gets ctxCwd
     -env <- procEnv
     -(exit, out, err) <- liftIO $ PB.readCreateProcessWithExitCode p { cwd = dir, env = env } input
     -case exit of
     -    ExitFailure code -> do
     -        trace <- gets ctxTrace
     -        throwError (trace, code)
     -    _ -> return (out, err)
     -}

runRecipe :: Recipe a -> IO ()
runRecipe recipe = do
    conf <- defRecipeConf
    runRecipeConf conf recipe

runRecipeConf :: RecipeConf -> Recipe a -> IO ()
runRecipeConf conf recipe = do
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
        Left trace@((result, ctx'):|_) -> do
            printTrace (recipeTraceLength conf) trace
            case result of
                StepSucceeded _   -> error "Recipe.runRecipeConf: invalid case"
                StepFailed _ code -> hPrintf stderr "Cook: process exited with code %d\n" code
                Failure msg       -> hPrintf stderr "Cook: step failed with %s\n" msg
            hPrintf stderr "      using %s\n" (show ctx' { ctxTrace = [] })
        Right _ | recipeConfVerbose conf -> hPutStrLn stderr "Cook: recipe successful"
                | otherwise              -> return ()

runPipe :: NonEmpty Step -> Recipe ()
runPipe pipe = runPipeRead pipe >>= liftIO . T.putStr

runPipeRead :: NonEmpty Step -> Recipe Text
runPipeRead pipe = runPipeTakeRead pipe T.empty

runPipeRead' :: NonEmpty Step -> Recipe ByteString
runPipeRead' pipe = runPipeTakeRead' pipe empty

runPipeTakeRead :: NonEmpty Step -> Text -> Recipe Text
runPipeTakeRead pipe input = T.decodeUtf8 <$> (runPipeTakeRead' pipe $ T.encodeUtf8 input)

runPipeTakeRead' :: NonEmpty Step -> ByteString -> Recipe ByteString
runPipeTakeRead' pipe input = build (NE.toList pipe) input
  where build []     input' = return input'
        build (s:ss) input' = do
            (out, _) <- runTakeRead' s input'
            build ss out

printTrace :: Int -> NonEmpty Trace -> IO ()
printTrace n (failed@(_, ctx):|prev) = do
    let name = maybe "" (" " ++) (showCtxRecipeName ctx)
    hPrintf stderr "Cook: error in recipe%s:\n" name
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take n prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, ctx') = let names = maybe "" (++ " ") (showCtxRecipeName ctx')
                           in printf "%s (%sfrom %s)" (show step) names (fromJust $ ctxCwd ctx')

showCtxRecipeName :: Ctx -> Maybe String
showCtxRecipeName Ctx { ctxRecipeNames = [] } = Nothing
showCtxRecipeName Ctx { ctxRecipeNames = ns } = Just $ intercalate "." $ reverse ns

getEnv :: String -> Recipe (Maybe String)
getEnv = liftIO . lookupEnv
