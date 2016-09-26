module Cook.Catalog.Arch.Rootfs (
    buildRootfs
  ) where

import Control.Monad.IO.Class
import System.Directory

import Cook.Recipe

buildRootfs :: FilePath -> Recipe FilePath
buildRootfs path = withRecipeName "Arch.Rootfs.BuildRootfs" $ do
    liftIO $ createDirectory path
    withSudo $ do
        -- XXX --noconfirm
        runProc "pacstrap" ["-i", "-c", "-d", path, "--needed", "base", "--ignore", "linux"]
    return path
