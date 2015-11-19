{-module FileMap where-}

--
-- TODO: Exports
-- TODO: Review code
-- TODO: Templates
--

--
-- TODO: how to set sudo user?
--

import Data.Default
import Data.Foldable

import Data.Text (Text)
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Posix.Types
import System.Posix.User

import qualified Data.Text.IO as T

type Attrs = (Maybe FileMode, Maybe (String, String))

data Content = Copy FilePath
             | Template -- TODO
             | Content Text

data FileMap = File FilePath Content Attrs
             | Dir FilePath Attrs [FileMap]
             | DirEmpty FilePath Attrs
             | Mode FilePath Attrs

defAttrs :: Attrs
defAttrs = def

apply :: FilePath -> FileMap -> IO ()
apply base filemap = do
    applyFile base filemap
    applyAttrs base filemap

applyFile :: FilePath -> FileMap -> IO ()
applyFile base (DirEmpty name _)   = mkdir (base </> name)
applyFile base (Dir name _ submap) = do
    mkdir (base </> name)
    mapM_ (apply $ base </> name) submap
applyFile base (File name (Content txt) _) = T.writeFile (base </> name) txt
applyFile base (File name (Copy from) _)   = copyFile from (base </> name)
applyFile _ (File _ Template _)            = undefined -- TODO
applyFile _ (Mode _ _)                     = return ()

applyAttrs :: FilePath -> FileMap -> IO ()
applyAttrs base (DirEmpty name attrs)   = useAttrs (base </> name) attrs
applyAttrs base (Dir name attrs submap) = do
    useAttrs (base </> name) attrs
    mapM_ (apply $ base </> name) submap
applyAttrs base (File name (Content _) attrs) = useAttrs (base </> name) attrs
applyAttrs base (File name (Copy _) attrs)    = useAttrs (base </> name) attrs
applyAttrs base (File name Template attrs)    = useAttrs (base </> name) attrs
applyAttrs base (Mode name attrs)             = useAttrs (base </> name) attrs

useAttrs :: FilePath -> Attrs -> IO ()
useAttrs _ (Nothing, Nothing) = return ()
useAttrs path (mode, perm)    = do
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
main :: IO ()
main = mapM_ (apply ".") [
    Dir "deploy/foo" defAttrs [
        File "fstab" (Copy "/etc/fstab") defAttrs
      , File "yyy" (Copy "/tmp/xxx") defAttrs
      , File "mtab" (Copy "/etc/mtab") defAttrs
      , DirEmpty "/tmp/fuuuuuuuuuur" (Just 0o0444, Nothing)
      , Mode "/tmp/fuuuuuuuuuur" (Just 0o0444, Nothing)
      ]
  , DirEmpty "caca" defAttrs
  ]
