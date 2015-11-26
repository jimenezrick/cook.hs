{-# LANGUAGE GADTs #-}

module FileMap (
    FileMap (..)
  , Content (..)

  , defAttrs
  , applyFileMap
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

import qualified Template as T

type Attrs = (Maybe FileMode, Maybe (String, String))

data Content where
    Copy     :: FilePath -> Content
    Template :: (Data a, Typeable a, Generic a, FromJSON a) => T.Template a -> Content
    Content  :: Text -> Content

data FileMap = File FilePath Content Attrs
             | Mode FilePath Attrs
             | Dir FilePath Attrs [FileMap]
             | DirEmpty FilePath Attrs

defAttrs :: Attrs
defAttrs = def

applyFileMap :: FilePath -> FileMap -> IO ()
applyFileMap base filemap = do
    applyFile base filemap
    applyAttrs base filemap

applyFile :: FilePath -> FileMap -> IO ()
applyFile base (File name (Content txt) _)   = Text.writeFile (base </> name) txt
applyFile base (File name (Copy src) _)      = copyFile src (base </> name)
applyFile base (File name (Template tmpl) _) = T.useTemplate tmpl (base </> name)
applyFile _ (Mode _ _)                       = return ()
applyFile base (DirEmpty name _)             = mkdir (base </> name)
applyFile base (Dir name _ submap)           = do
    mkdir (base </> name)
    mapM_ (applyFileMap $ base </> name) submap

applyAttrs :: FilePath -> FileMap -> IO ()
applyAttrs base (File name _ attrs)     = useAttrs (base </> name) attrs
applyAttrs base (Mode name attrs)       = useAttrs (base </> name) attrs
applyAttrs base (DirEmpty name attrs)   = useAttrs (base </> name) attrs
applyAttrs base (Dir name attrs submap) = do
    useAttrs (base </> name) attrs
    mapM_ (applyFileMap $ base </> name) submap

useAttrs :: FilePath -> Attrs -> IO ()
useAttrs path (mode, perm) = do
    forM_ mode (chmod path)
    forM_ perm (uncurry $ chown path)

mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing True

chmod :: FilePath -> FileMode -> IO ()
chmod path mode | mode > 0o7777 = error "FileMap.chmod: invalid mode"
                | otherwise     = setFileMode path mode

chown :: FilePath -> String -> String -> IO ()
chown path user group = do
    uentry <- getUserEntryForName user
    gentry <- getGroupEntryForName group
    setOwnerAndGroup path (userID uentry) (groupID gentry)

-------------------------------------------------------------------------
-- main :: IO ()
-- main = mapM_ (applyFileMap ".") [
--     Dir "deploy/foo" defAttrs [
--         File "fstab" (Copy "/etc/fstab") defAttrs
--       , File "yyy" (Copy "/tmp/xxx") defAttrs
--       , File "mtab" (Copy "/etc/mtab") defAttrs
--       , DirEmpty "/tmp/fuuuuuuuuuur" (Just 0o0444, Nothing)
--       , Mode "/tmp/fuuuuuuuuuur" (Just (0o0444), Just ("ricardo", "users"))
--       , File "template" (Template (T.asTemplate "p.mustache" :: T.Template T.Foo)) defAttrs
--       ]
--   , DirEmpty "caca" defAttrs
--   ]
