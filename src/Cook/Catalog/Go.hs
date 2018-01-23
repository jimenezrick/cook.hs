module Cook.Catalog.Go (
    goGet
  ) where

import System.FilePath ((</>), takeBaseName)

import Cook.Recipe
import Cook.Recipe.PkgManager
import Cook.Recipe.Util

goGet :: String -> Recipe f ()
goGet repo = withRecipeName "Go.Get" $ do
    requirePackages ["go", "git"]
    withCdTempDir $ \tmpDir -> do
        withEnv [("GOPATH", tmpDir)] $ do
            runProc "go" ["get", repo]
        runProc "cp" [tmpDir </> "bin" </> takeBaseName repo, "/usr/local/bin"]
