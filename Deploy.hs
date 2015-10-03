{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Tree
import Control.Monad.Reader
import System.Directory
import System.FilePath
import System.Process

data Env a = Env a

type ReceiptEnv a = ReaderT (Env a) IO


main :: IO ()
main = return ()



foo :: ReceiptEnv a a
foo = do Env a <- ask
         return a

data Owner = KeepOwner -- | ...

data Mode = KeepMode -- | ...

data Step = Cp -- XXX
          | CpR -- XXX
          | Mv -- XXX
          | MkDir FilePath
          | ChOwnMod Owner Mode
          | Templ FilePath FilePath
          | Cmd String

type Recipe = [Step]

type DirTree = Tree FilePath

mkDirTree = undefined

-- ?
templateBase :: IO FilePath
templateBase = undefined

files =
    [ Cp -- "foo.conf" "/etc" DefMode
    , Mv -- "foo.conf" "/etc" PreserveMode
    , Templ "bar.conf.tmpl" "/etc"
    ]





runStep :: MonadIO m => Step -> m ()
runStep (MkDir dir) = liftIO $ callProcess "mkdir" ["-p", dir]
runStep (Cmd cmd) = liftIO $ callCommand cmd
runStep (Templ cmd) = liftIO $ callCommand cmd

runRecipe :: MonadIO m => Recipe -> m ()
runRecipe recp = mapM_ runStep recp


resolveSrcPath :: FilePath -> FilePath
resolveSrcPath path | isAbsolute path = path
                    | otherwise       = "files" </> path

