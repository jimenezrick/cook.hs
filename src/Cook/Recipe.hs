module Cook.Recipe
  ( Recipe
  , RecipeConf (..)
  , Step (..)

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
  , withoutError
  , withoutErrorWhen
  , withSudo
  , withSudoUser

  , defRecipeConf
  , defRecipeConfCustomFacts
  , recipeConf
  , runRecipe
  , runRecipeCustomFacts
  , runRecipeConf
  , runRecipeConfEither
  , printRecipeFailure
  , failWith
  , catchException
  , recipeIO

  , FT.FsTree (..)
  , FT.Content (..)

  , FT.defAttrs
  , createFsTree

  , getFacts
  , getEnv
  , getCwd
  ) where

import Control.Arrow
import Control.Exception.Safe (MonadCatch, MonadThrow, SomeException, handle)
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.ByteString.Lazy (ByteString, empty)
import Data.Char (toUpper)
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Text.Lazy (Text, unpack)
import Network.BSD
import Safe (headMay)
import System.Directory
import System.Environment (getEnvironment, lookupEnv)
import System.Exit
import System.FilePath
import System.IO
import Text.Pretty.Simple (pShowNoColor)
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Process.Typed as P

import qualified Cook.Facts as F
import qualified Cook.Recipe.FsTree as FT

data RecipeConf f where
    RecipeConf :: Show f =>
        { recipeConfFacts    :: F.Facts f
        , recipeConfDebug    :: Bool
        , recipeConfVerbose  :: Bool
        , recipeTraceLength  :: Int
        , recipeConfRootDir  :: FilePath
        , recipeConfHostName :: String
        } -> RecipeConf f

deriving instance Show (RecipeConf f)

data ExecMode = ExecLocal
              | ExecSsh String String
              deriving Show

data ExecPriv = ExecNormal
              | ExecSudo (Maybe String)
              deriving Show

type TraceStep f = (Result, Ctx f)

data Ctx f where
    Ctx :: Show f =>
        { ctxRecipeNames :: [String]
        , ctxCwd         :: Maybe FilePath
        , ctxEnv         :: Maybe [(String, String)]
        , ctxMode        :: ExecMode
        , ctxTrace       :: [TraceStep f]
        , ctxPriv        :: ExecPriv
        , ctxRecipeConf  :: RecipeConf f
        } -> Ctx f

deriving instance Show (Ctx f)

type RecipeFailTrace f = NonEmpty (TraceStep f)

newtype SafeIO a = SafeIO
    { getIO :: IO a
    } deriving (Functor, Applicative, Monad, MonadBase IO, MonadThrow, MonadCatch)

instance MonadBaseControl IO SafeIO where
    type StM SafeIO a = a
    liftBaseWith f = SafeIO $ liftBaseWith $ \runInBase -> f (runInBase . getIO)
    restoreM = SafeIO . return

type Recipe f = ExceptT (RecipeFailTrace f) (StateT (Ctx f) SafeIO)

data Step = Proc FilePath [String]
          | Shell String

instance Show Step where
    show (Proc prog args) = printf "process: \"%s %s\"" prog (unwords args)
    show (Shell cmd)      = printf "shell:   \"%s\"" cmd

data Result = StepSucceeded Step
            | StepFailed Step Int
            | Failure String
            deriving Show

data IORedir = In ByteString
             | Out
             | InOut ByteString
             deriving Show

defRecipeConf :: IO (RecipeConf ())
defRecipeConf = defRecipeConfCustomFacts ()

defRecipeConfCustomFacts :: Show f => f -> IO (RecipeConf f)
defRecipeConfCustomFacts customFacts = do
    facts <- F.grabFacts customFacts
    root <- getCurrentDirectory
    hostname <- getHostName
    return RecipeConf {
        recipeConfFacts    = facts
      , recipeConfDebug    = False
      , recipeConfVerbose  = True
      , recipeTraceLength  = 5
      , recipeConfRootDir  = root
      , recipeConfHostName = hostname
      }

recipeConf :: Recipe f (RecipeConf f)
recipeConf = gets ctxRecipeConf

procEnv :: Recipe f (Maybe [(String, String)])
procEnv = do
    env <- gets ctxEnv
    case env of
        Nothing   -> return Nothing
        Just env' -> do
            penv <- recipeIO getEnvironment
            return . Just . M.toList . M.fromList $ (penv ++ env')

createFsTree :: FilePath -> FT.FsTree -> Recipe f ()
createFsTree base fstree = recipeIO $ FT.createFsTree base fstree

proc :: FilePath -> [String] -> Step
proc = Proc

proc0 :: FilePath -> Step
proc0 prog = Proc prog []

sh :: String -> Step
sh = Shell

failWith :: String -> Recipe f a
failWith msg = do
    traceResult $ Failure msg
    abortRecipe

abortRecipe :: Recipe f a
abortRecipe = gets ctxTrace >>= throwError . NE.fromList

catchException :: Recipe f a -> Recipe f a
catchException = handle (\e -> failWith $ show (e :: SomeException))

withCtx :: (Ctx f -> Ctx f) -> Recipe f a -> Recipe f a
withCtx f recipe = do
    ctx <- get
    (trace, a) <- hoist (withStateT f) $ do
        ctx' <- get
        let conf = ctxRecipeConf ctx'
        when (recipeConfDebug conf) $
            recipeIO $ hPrintf stderr "Cook: using %s\n" (pShowNoColor ctx')
        a <- recipe
        trace <- gets ctxTrace
        return (trace, a)
    put ctx { ctxTrace = trace }
    return a

withRecipeName :: String -> Recipe f a -> Recipe f a
withRecipeName name recipe = withCtx (\ctx@Ctx {..} -> ctx { ctxRecipeNames = addRecipeName ctxRecipeNames casedName }) $ do
    ctx <- get
    let conf = ctxRecipeConf ctx
    case showCtxRecipeName ctx of
        Just recipeName
          | recipeConfVerbose conf -> recipeIO $ hPrintf stderr "Cook: in %s\n" recipeName
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

withCd :: FilePath -> Recipe f a -> Recipe f a
withCd dir = withCtx (chdir dir)
  where chdir to | isAbsolute to = \ctx -> ctx { ctxCwd = Just to }
                 | otherwise     = \ctx ->
                     case ctxCwd ctx of
                         Nothing  -> ctx { ctxCwd = Just to }
                         Just cur -> ctx { ctxCwd = Just $ cur </> to }

withEnv :: [(String, String)] -> Recipe f a -> Recipe f a
withEnv env = withCtx (\ctx -> ctx { ctxEnv = Just env })

withoutError :: Recipe f a -> Recipe f (Either Result a)
withoutError recipe = (Right <$> recipe) `catchError` \trace -> return . Left . fst $ NE.head trace

withoutErrorWhen :: (Int -> Maybe a) -> Recipe f a -> Recipe f a
withoutErrorWhen isCode recipe = do
    result <- withoutError recipe
    case result of
        Left (StepSucceeded _ )   -> error "Recipe.withoutErrorWhen: invalid pattern"
        Left (StepFailed _ code)
          | Just v <- isCode code -> return v
          | otherwise             -> abortRecipe
        Left (Failure _)          -> abortRecipe
        Right v                   -> return v

withSudo :: Recipe f a -> Recipe f a
withSudo = withCtx $ \ctx -> ctx { ctxPriv = ExecSudo Nothing }

withSudoUser :: String -> Recipe f a -> Recipe f a
withSudoUser user | null user = error "Recipe.withSudoUser: empty user"
                  | otherwise = withCtx $ \ctx -> ctx { ctxPriv = ExecSudo (Just user) }

getFacts :: Recipe f (F.Facts f)
getFacts = gets $ recipeConfFacts . ctxRecipeConf

withSsh :: String -> String -> Recipe f a -> Recipe f a
withSsh user host recipe = withCtx (\ctx -> ctx { ctxMode = ExecSsh user host }) recipe

getCwd :: Recipe f FilePath
getCwd = do
    dir <- gets ctxCwd
    case dir of
        Nothing  -> recipeIO getCurrentDirectory
        Just d -> recipeIO $ canonicalizePath d

traceResult :: Result -> Recipe f ()
traceResult result = do
    dir <- getCwd
    modify $ \ctx@Ctx {..} -> ctx { ctxTrace = (result, ctx { ctxCwd = Just dir }):ctxTrace }

buildProCfg :: ExecMode -> ExecPriv -> Step -> P.ProcessConfig () () ()
buildProCfg ExecLocal priv (Proc prog args) = uncurry P.proc $ buildProcProg priv prog args
buildProCfg ExecLocal priv (Shell cmd)      = P.shell $ buildShellCmd priv cmd
buildProCfg (ExecSsh user host) priv step   = buildSshCmd user host priv step

buildSshCmd :: String -> String -> ExecPriv -> Step -> P.ProcessConfig () () ()
buildSshCmd user host priv (Proc prog args) = P.proc "ssh" $ [user ++ "@" ++ host, prog'] ++ args'
  where (prog', args') = buildProcProg priv prog args
buildSshCmd user host priv (Shell cmd) = P.proc "ssh" $ [user ++ "@" ++ host, cmd']
  where cmd' = buildShellCmd priv cmd

buildProcProg :: ExecPriv -> FilePath -> [String] -> (FilePath, [String])
buildProcProg ExecNormal             prog args = (prog, args)
buildProcProg (ExecSudo Nothing)     prog args = ("sudo", prog:args)
buildProcProg (ExecSudo (Just user)) prog args = ("sudo", ["-u", user] ++ prog:args)

buildShellCmd :: ExecPriv -> String -> String
buildShellCmd priv = case priv of
    ExecNormal           -> strictMode
    ExecSudo Nothing     -> strictMode . sudo
    ExecSudo (Just user) -> strictMode . sudoUser user
  where strictMode = ("set -o errexit -o nounset -o pipefail; " ++)
        sudo       = ("sudo " ++)
        sudoUser u = (("sudo -u " ++ u ++ " " :: String) ++)

run :: Step -> Recipe f ()
run = void . run' Nothing

run' :: Maybe IORedir -> Step -> Recipe f (Maybe (ByteString, ByteString))
run' ioredir step = do
    dir <- gets ctxCwd
    env <- procEnv
    mode <- gets ctxMode
    sudo <- gets ctxPriv

    let setCwd   = maybe id P.setWorkingDir dir -- FIXME: withCd broken, use execcwd.sh
        setEnv   = maybe id P.setEnv env -- FIXME: Put them as: ssh user@host 'FOO="BAR BUR"' prog args...?
        setStdin = P.setStdin . P.byteStringInput
        pCfg     = setCwd . setEnv $ buildProCfg mode sudo step

    (exit, outErr) <- case ioredir of
        Nothing -> do
            exit <- recipeIO $ P.runProcess pCfg
            return (exit, Nothing)
        Just (In bin) -> do
            exit <- recipeIO . P.runProcess $ setStdin bin pCfg
            return (exit, Nothing)
        Just (InOut bin) -> do
            (exit, out, err) <- recipeIO . P.readProcess $ setStdin bin pCfg
            return (exit, Just (out, err))
        Just Out -> do
            (exit, out, err) <- recipeIO $ P.readProcess pCfg
            return (exit, Just (out, err))
    case exit of
        ExitFailure code -> traceResult (StepFailed step code) >> abortRecipe
        ExitSuccess      -> traceResult $ StepSucceeded step
    return outErr

runProc :: FilePath -> [String] -> Recipe f ()
runProc = (fmap . fmap) run proc

runProc0 :: FilePath -> Recipe f ()
runProc0 = run . proc0

runSh :: String -> Recipe f ()
runSh = run . sh

runInB :: ByteString -> Step -> Recipe f ()
runInB bin step = void $ run' (Just $ In bin) step

runOutB :: Step -> Recipe f (ByteString, ByteString)
runOutB step = fromJust <$> run' (Just Out) step

runInOutB :: ByteString -> Step -> Recipe f (ByteString, ByteString)
runInOutB bin step = fromJust <$> run' (Just $ InOut bin) step

runIn :: Text -> Step -> Recipe f ()
runIn = runInB . T.encodeUtf8

runOut :: Step -> Recipe f (Text, Text)
runOut step = (T.decodeUtf8 *** T.decodeUtf8) <$> runOutB step

runInOut :: Text -> Step -> Recipe f (Text, Text)
runInOut tin step = (T.decodeUtf8 *** T.decodeUtf8) <$> runInOutB (T.encodeUtf8 tin) step

runRecipe :: Recipe () a -> IO ()
runRecipe recipe = do
    conf <- defRecipeConf
    runRecipeConf conf recipe

runRecipeCustomFacts :: Show f => f -> Recipe f a -> IO ()
runRecipeCustomFacts customFacts recipe = do
    conf <- defRecipeConfCustomFacts customFacts
    runRecipeConf conf recipe

runRecipeConf :: RecipeConf f -> Recipe f a -> IO ()
runRecipeConf conf recipe = do
    when (recipeConfVerbose conf) $
        hPrintf stderr "Cook: running with %s\n" (pShowNoColor conf)
    r <- runRecipeConfEither conf recipe
    case r of
        Left failure                     -> printRecipeFailure conf failure
        Right _ | recipeConfVerbose conf -> hPutStrLn stderr "Cook: recipe successful"
                | otherwise              -> return ()

runRecipeConfEither :: RecipeConf f -> Recipe f a -> IO (Either (RecipeFailTrace f) a)
runRecipeConfEither conf@(RecipeConf {..}) recipe =
    let ctx = Ctx {
        ctxRecipeNames = []
      , ctxCwd         = Nothing
      , ctxEnv         = Nothing
      , ctxMode        = ExecLocal
      , ctxTrace       = mempty
      , ctxPriv        = ExecNormal
      , ctxRecipeConf  = conf
      }
    in getIO . flip evalStateT ctx $ runExceptT recipe

printRecipeFailure :: RecipeConf f -> RecipeFailTrace f -> IO ()
printRecipeFailure conf trace@((result, ctx'):|_) = do
    printRecipeTrace (recipeTraceLength conf) trace
    case result of
        StepSucceeded _   -> error "Recipe.runRecipeConf: invalid pattern"
        StepFailed _ code -> hPrintf stderr "Cook: process exited with code %d\n" code
        Failure msg       -> hPrintf stderr "Cook: step failed because: %s\n" msg
    hPrintf stderr "      using %s\n" (pShowNoColor ctx' { ctxTrace = [] })

runPipe :: NonEmpty Step -> Recipe f ()
runPipe pipe = runPipeOut pipe >>= recipeIO . T.putStr

runPipeOut :: NonEmpty Step -> Recipe f Text
runPipeOut = runPipeInOut T.empty

runPipeOutB :: NonEmpty Step -> Recipe f ByteString
runPipeOutB = runPipeInOutB empty

runPipeInOut :: Text -> NonEmpty Step -> Recipe f Text
runPipeInOut input pipe = T.decodeUtf8 <$> runPipeInOutB (T.encodeUtf8 input) pipe

runPipeInOutB :: ByteString -> NonEmpty Step -> Recipe f ByteString
runPipeInOutB input pipe = build (NE.toList pipe) input
  where build []     input' = return input'
        build (s:ss) input' = do
            (out, _) <- runInOutB input' s
            build ss out

printRecipeTrace :: Int -> RecipeFailTrace f -> IO ()
printRecipeTrace n (failed@(_, ctx):|prev) = do
    let name = maybe "" (" " ++) (showCtxRecipeName ctx)
    hPrintf stderr "Cook: error in recipe%s:\n" name
    mapM_ (hPutStrLn stderr . ("  " ++) . fmt) $ reverse $ take n prev
    hPutStrLn stderr $ "> " ++ fmt failed
  where fmt (step, ctx') = let names = maybe "" (++ " ") (showCtxRecipeName ctx')
                           in printf "%s (%sfrom %s%s%s)" (show step) names (fromJust $ ctxCwd ctx') (fmtPriv ctx') (fmtMode ctx')
        fmtPriv ctx' = case ctxPriv ctx' of
                           ExecNormal           -> ""
                           ExecSudo (Just user) -> printf " as sudo:%s" user :: String
                           ExecSudo Nothing     -> printf " as sudo" :: String
        fmtMode ctx' = case ctxMode ctx' of
                           ExecLocal         -> ""
                           ExecSsh user host -> printf " in ssh:%s@%s" user host :: String

showCtxRecipeName :: Ctx f -> Maybe String
showCtxRecipeName Ctx { ctxRecipeNames = [] } = Nothing
showCtxRecipeName Ctx { ctxRecipeNames = ns } = Just $ intercalate "." $ reverse ns

getEnv :: String -> Recipe f (Maybe String)
getEnv = recipeIO . lookupEnv

recipeIO :: IO a -> Recipe f a
recipeIO = catchException . lift . lift . SafeIO
