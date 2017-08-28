module Cook.Catalog.Go (
    goGet
  ) where

import Control.Lens
import System.FilePath ((</>), takeBaseName)

import Cook.Provider
import Cook.Recipe
import Cook.Recipe.Util

goGet :: String -> Recipe f ()
goGet repo = withRecipeName "Go.Get" $ do
    prov <- getProvider
    prov^.pkgManager.requirePackages $ ["go", "git"]
    withTempDir $ \tmpDir -> do
        withEnv [("GOPATH", tmpDir)] $ do
            runProc "go" ["get", repo]
        runProc "cp" [tmpDir </> "bin" </> takeBaseName repo, "/usr/local/bin"]
