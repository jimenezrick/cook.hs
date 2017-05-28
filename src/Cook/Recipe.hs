module Cook.Recipe (
    Recipe
  , RecipeConf (..)
  , Step

  , proc
  , proc0
  , sh

  , run
  , runProc
  , runProc0
  , runSh

  , runInB
  , runOutB
  , runInOutB
  , runIn
  , runOut
  , runInOut

  , runPipe
  , runPipeOut
  , runPipeOutB
  , runPipeInOut
  , runPipeInOutB

  , withRecipeName
  , withCd
  , withEnv
  , withSsh
  , withSshUser
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

import Control.Arrow
import Control.Exception.Lifted
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Data.ByteString.Lazy (ByteString, empty)
import Data.Char (toUpper)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text.Lazy (Text)
import Network.BSD
import Safe (headMay)
import System.Directory
import System.Environment (lookupEnv, getEnvironment)
import System.Exit
import System.FilePath
import System.IO
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Process.Typed as P

import qualified Cook.Recipe.FsTree as F

data RecipeConf = RecipeConf {
    recipeConfDebug    :: Bool
  , recipeConfVerbose  :: Bool
  , recipeTraceLength  :: Int
  , recipeConfRootDir  :: FilePath
  , recipeConfHostName :: String
  } deriving Show

data ExecTarget = ExecLocal
                | ExecSsh (Maybe String) String
                deriving Show

data ExecPrivileges = ExecNormal
                    | ExecSudo (Maybe String)
                    deriving Show

type TraceStep = (Result, Ctx)

data Ctx = Ctx {
    ctxRecipeNames :: [String]
  , ctxCwd         :: Maybe FilePath
  , ctxEnv         :: Maybe [(String, String)]
  , ctxTarget      :: ExecTarget
  , ctxTrace       :: [TraceStep]
  , ctxSudo        :: ExecPrivileges
  , ctxRecipeConf  :: RecipeConf
  } deriving Show

type Recipe = ExceptT (NonEmpty TraceStep) (StateT Ctx IO)

data Step = Proc FilePath [String]
          | Shell String

instance Show Step where
    show (Proc prog args) = printf "process: %s %s" prog (unwords args)
    show (Shell cmd)      = printf "shell:   %s" cmd

data Result = StepSucceeded Step
            | StepFailed Step Int
            | Failure String
            deriving Show

data IORedir = In ByteString
             | Out
             | InOut ByteString
             deriving Show

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

proc0 :: FilePath -> Step
proc0 prog = Proc prog []

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
withRecipeName name recipe = withCtx (\ctx@Ctx {..} -> ctx { ctxRecipeNames = addRecipeName ctxRecipeNames casedName }) $ do
    ctx <- get
    let conf = ctxRecipeConf ctx
    case showCtxRecipeName ctx of
        Just recipeName
          | recipeConfVerbose conf -> liftIO $ hPrintf stderr "Cook: in %s\n" recipeName
        _                          -> return ()
    recipe
  where up ""     = ""
        up (n:ns) = toUpper n : ns
        casedName = intercalate "." . map up $ splitOn "." name

addRecipeName :: [String] -> String -> [String]
addRecipeName []   name   = [name]
addRecipeName prev name
  | Just s <- commonScope = intercalate "." (splitOn "." name \\ s) : prev
  | otherwise             = name:prev
  where commonScope = headMay $ dropWhile (not . (`isPrefixOf` splitOn "." name)) scopes
        scopes      = concatMap (reverse . tail . inits . splitOn ".") prev

withCd :: FilePath -> Recipe a -> Recipe a
withCd dir = withCtx (chdir dir)
  where chdir to | isAbsolute to = \ctx -> ctx { ctxCwd = Just to }
                 | otherwise     = \ctx ->
                     case ctxCwd ctx of
                         Nothing  -> ctx { ctxCwd = Just to }
                         Just cur -> ctx { ctxCwd = Just $ cur </> to }

withEnv :: [(String, String)] -> Recipe a -> Recipe a
withEnv env = withCtx (\ctx -> ctx { ctxEnv = Just env })

withSsh :: String -> Recipe a -> Recipe a
withSsh = withSsh' Nothing

withSsh' :: Maybe String -> String -> Recipe a -> Recipe a
withSsh' user host = withCtx (\ctx -> ctx { ctxTarget = ExecSsh user host })

withSshUser :: String -> String -> Recipe a -> Recipe a
withSshUser user = withSsh' (Just user)

withoutError :: Recipe a -> Recipe (Either Result a)
withoutError recipe = (Right <$> recipe) `catchError` \trace -> return . Left . fst $ NE.head trace

withoutErrorWhen :: (Int -> Maybe a) -> Recipe a -> Recipe a
withoutErrorWhen isCode recipe = do
    result <- withoutError recipe
    case result of
        Left (StepSucceeded _ )   -> error "Recipe.withoutErrorWhen: invalid pattern"
        Left (StepFailed _ code)
          | Just v <- isCode code -> return v
          | otherwise             -> abortRecipe
        Left (Failure _)          -> abortRecipe
        Right v                   -> return v

withSudo :: Recipe a -> Recipe a
withSudo = withCtx $ \ctx -> ctx { ctxSudo = ExecSudo Nothing }

withSudoUser :: String -> Recipe a -> Recipe a
withSudoUser user | null user = error "Recipe.withSudoUser: empty user"
                  | otherwise = withCtx $ \ctx -> ctx { ctxSudo = ExecSudo (Just user) }

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

buildProCfg :: ExecTarget -> ExecPrivileges -> Step -> P.ProcessConfig () () ()
buildProCfg ExecLocal           priv (Proc prog args) = uncurry P.proc $ buildProcProg priv prog args
buildProCfg ExecLocal           priv (Shell cmd)      = P.shell $ buildShellCmd priv cmd
buildProCfg (ExecSsh user host) priv (Shell cmd)      = uncurry P.proc $ buildSshCmd user host [buildShellCmd priv cmd]
buildProCfg (ExecSsh user host) priv (Proc prog args) = uncurry P.proc $ buildSshCmd user host (prog':args')
  where (prog', args') = buildProcProg priv prog args

buildProcProg :: ExecPrivileges -> FilePath -> [String] -> (FilePath, [String])
buildProcProg ExecNormal             prog args = (prog, args)
buildProcProg (ExecSudo Nothing)     prog args = ("sudo", prog:args)
buildProcProg (ExecSudo (Just user)) prog args = ("sudo", ["-u", user] ++ prog:args)

buildShellCmd :: ExecPrivileges -> String -> String
buildShellCmd sudoMode = case sudoMode of
    ExecNormal           -> strictMode
    ExecSudo Nothing     -> strictMode . sudo
    ExecSudo (Just user) -> strictMode . sudoUser user
  where strictMode = ("set -o errexit -o nounset -o pipefail; " ++)
        sudo       = ("sudo " ++)
        sudoUser u = (("sudo -u " ++ u ++ " " :: String) ++)

buildSshCmd :: Maybe String -> String -> [String] -> (FilePath, [String])
buildSshCmd (Just user) host args = ("ssh", (user ++ "@" ++ host):args)
buildSshCmd Nothing     host args = ("ssh", host:args)

run :: Step -> Recipe ()
run = void . run' Nothing

run' :: Maybe IORedir -> Step -> Recipe (Maybe (ByteString, ByteString))
run' ioredir step = do
    dir <- gets ctxCwd
    env <- procEnv
    target <- gets ctxTarget
    sudo <- gets ctxSudo

    let setCwd   = maybe id P.setWorkingDir dir -- FIXME: Not in SSH, user 'cd dir; exec prog'
        setEnv   = maybe id P.setEnv env -- FIXME: Not in SSH, user SendEnv
        setStdin = P.setStdin . P.byteStringInput
        pCfg     = setCwd . setEnv $ buildProCfg target sudo step

    (exit, outErr) <- case ioredir of
        Nothing -> do
            exit <- liftIO $ P.runProcess pCfg
            return (exit, Nothing)
        Just (In bin) -> do
            exit <- liftIO . P.runProcess $ setStdin bin pCfg
            return (exit, Nothing)
        Just (InOut bin) -> do
            (exit, out, err) <- liftIO . P.readProcess $ setStdin bin pCfg
            return (exit, Just (out, err))
        Just Out -> do
            (exit, out, err) <- liftIO $ P.readProcess pCfg
            return (exit, Just (out, err))
    case exit of
        ExitFailure code -> traceResult (StepFailed step code) >> abortRecipe
        ExitSuccess      -> traceResult $ StepSucceeded step
    return outErr

runProc :: FilePath -> [String] -> Recipe ()
runProc = (fmap . fmap) run proc

runProc0 :: FilePath -> Recipe ()
runProc0 = run . proc0

runSh :: String -> Recipe ()
runSh = run . sh

runInB :: ByteString -> Step -> Recipe ()
runInB bin step = void $ run' (Just $ In bin) step

runOutB :: Step -> Recipe (ByteString, ByteString)
runOutB step = fromJust <$> run' (Just Out) step

runInOutB :: ByteString -> Step -> Recipe (ByteString, ByteString)
runInOutB bin step = fromJust <$> run' (Just $ InOut bin) step

runIn :: Text -> Step -> Recipe ()
runIn = runInB . T.encodeUtf8

runOut :: Step -> Recipe (Text, Text)
runOut step = (T.decodeUtf8 *** T.decodeUtf8) <$> runOutB step

runInOut :: Text -> Step -> Recipe (Text, Text)
runInOut tin step = (T.decodeUtf8 *** T.decodeUtf8) . fromJust <$> run' (Just . In $ T.encodeUtf8 tin) step

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
      , ctxTarget      = ExecLocal
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
                StepSucceeded _   -> error "Recipe.runRecipeConf: invalid pattern"
                StepFailed _ code -> hPrintf stderr "Cook: process exited with code %d\n" code
                Failure msg       -> hPrintf stderr "Cook: step failed because: %s\n" msg
            hPrintf stderr "      using %s\n" (show ctx' { ctxTrace = [] })
        Right _ | recipeConfVerbose conf -> hPutStrLn stderr "Cook: recipe successful"
                | otherwise              -> return ()

runPipe :: NonEmpty Step -> Recipe ()
runPipe pipe = runPipeOut pipe >>= liftIO . T.putStr

runPipeOut :: NonEmpty Step -> Recipe Text
runPipeOut = runPipeInOut T.empty

runPipeOutB :: NonEmpty Step -> Recipe ByteString
runPipeOutB = runPipeInOutB empty

runPipeInOut :: Text -> NonEmpty Step -> Recipe Text
runPipeInOut input pipe = T.decodeUtf8 <$> runPipeInOutB (T.encodeUtf8 input) pipe

runPipeInOutB :: ByteString -> NonEmpty Step -> Recipe ByteString
runPipeInOutB input pipe = build (NE.toList pipe) input
  where build []     input' = return input'
        build (s:ss) input' = do
            (out, _) <- runInOutB input' s
            build ss out

printTrace :: Int -> NonEmpty TraceStep -> IO ()
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
