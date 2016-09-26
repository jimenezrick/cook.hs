module Cook.Catalog.Systemd.Container (
    tarRootfs
  ) where

import System.FilePath

import Cook.Recipe

tarRootfs :: FilePath -> Recipe FilePath
tarRootfs path = withRecipeName "Systemd.Container.TarRootfs" $ do
    let tarball = path <.> "tar.gz"
    withSudo $ do
        runProc "tar" ["cfz", tarball, path, "-C", path]
        runProc "rm" ["-r", path]
    return tarball
