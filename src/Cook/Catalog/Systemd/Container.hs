module Cook.Catalog.Systemd.Container (
    tarRootfs
  , createCookDir
  ) where

import System.FilePath

import Cook.Recipe

tarRootfs :: FilePath -> Recipe FilePath
tarRootfs path = withRecipeName "Systemd.Container.TarRootfs" $ do
    let tarball = path <.> "tar.gz"
    runProc "tar" ["cfz", tarball, "-C", path, "."]
    runProc "rm" ["-r", path]
    return tarball

createEmbeddedCookDir :: FilePath -> Recipe ()
createEmbeddedCookDir containerPath = withRecipeName "Systemd.Container.CreateEmbeddedCookDir" $ do
    withCd containerPath $ do
        createFsTree "/" $ DirEmpty "cook/bin" (Just 0o700, Just ("root", "root"))

embedCabalBin :: FilePath -> String -> Recipe ()
embedCookBin containerPath prog = withRecipeName "Systemd.Container.EmbedCabalBin" $ do
    runProc "cabal" ["install", "--bindir=" ++ bindir, prog]
  where bindir = containerPath </> "cook/bin"

embedInCookDir :: FilePath -> FsTree -> Recipe ()
embedInCookDir containerPath fstree = withRecipeName "Systemd.Container.EmbedInCookDir" $ do
    createFsTree containerPath fstree
