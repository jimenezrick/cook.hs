module Cook.Catalog.Systemd.Container (
    compressRootfs
  ) where

import System.FilePath

import Cook.Recipe

compressRootfs :: FilePath -> Recipe FilePath
compressRootfs path = withRecipeName "Systemd.Container.CompressRootfs" $ do
    let tarball = path <.> "tar.xz"
    withSudo $ do
        withEnv [("XZ_OPT", "-9")] $ do
            runProc "tar" ["cfJ", tarball, path, "-C", path]
    return tarball
