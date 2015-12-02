module Cook.Recipe.FsTree (
    FsTree (..)
  , Content (..)

  , defAttrs
  , createFsTree
  ) where

import Data.Aeson
import Data.Data
import Data.Default
import Data.Foldable
import Data.Text (Text)
import GHC.Generics
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

import qualified Data.Text.IO as Text

import qualified Cook.Recipe.Template as T

type Attrs = (Maybe FileMode, Maybe (String, String))

data Content where
    Copy     :: FilePath -> Content
    Template :: (Data a, Typeable a, Generic a, FromJSON a) => T.Template a -> Content
    Content  :: Text -> Content

data FsTree = File FilePath Content Attrs
            | Mode FilePath Attrs
            | Dir FilePath Attrs [FsTree]
            | DirEmpty FilePath Attrs

defAttrs :: Attrs
defAttrs = def

createFsTree :: FilePath -> FsTree -> IO ()
createFsTree base fstree = do
    createFile base fstree
    applyAttrs base fstree

createFile :: FilePath -> FsTree -> IO ()
createFile base (File name (Content txt) _)   = Text.writeFile (base </> name) txt
createFile base (File name (Copy src) _)      = copyFile src (base </> name)
createFile base (File name (Template tmpl) _) = T.useTemplate tmpl (base </> name)
createFile _ (Mode _ _)                       = return ()
createFile base (DirEmpty name _)             = mkdir (base </> name)
createFile base (Dir name _ subtree)          = do
    mkdir (base </> name)
    mapM_ (createFile $ base </> name) subtree

applyAttrs :: FilePath -> FsTree -> IO ()
applyAttrs base (File name _ attrs)      = useAttrs (base </> name) attrs
applyAttrs base (Mode name attrs)        = useAttrs (base </> name) attrs
applyAttrs base (DirEmpty name attrs)    = useAttrs (base </> name) attrs
applyAttrs base (Dir name attrs subtree) = do
    useAttrs (base </> name) attrs
    mapM_ (applyAttrs $ base </> name) subtree

useAttrs :: FilePath -> Attrs -> IO ()
useAttrs path (mode, perm) = do
    forM_ mode (chmod path)
    forM_ perm (uncurry $ chown path)

mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

chmod :: FilePath -> FileMode -> IO ()
chmod path mode | mode > 0o7777 = error "FsTree.chmod: invalid mode"
                | otherwise     = setFileMode path mode

chown :: FilePath -> String -> String -> IO ()
chown path user group = do
    uentry <- getUserEntryForName user
    gentry <- getGroupEntryForName group
    setOwnerAndGroup path (userID uentry) (groupID gentry)
