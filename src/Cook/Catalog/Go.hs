module Cook.Catalog.Go (
    goGet
  ) where

import System.FilePath ((</>), takeBaseName)

import Cook.Recipe
import Cook.Recipe.Util
import Cook.Catalog.Arch.Pacman

goGet :: String -> Recipe ()
goGet repo = withRecipeName "Go.Get" $ do
    requirePackages ["go", "git"]
    withTempDir $ \tmpDir -> do
        withEnv [("GOPATH", tmpDir)] $ do
            runProc "go" ["get", repo]
        runProc "cp" [tmpDir </> "bin" </> takeBaseName repo, "/usr/local/bin"]
