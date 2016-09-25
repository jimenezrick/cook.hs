module Cook.Catalog.Go (
    goGet
  ) where

import System.FilePath ((</>))

import Cook.Recipe
import Cook.Recipe.Util
import Cook.Catalog.Arch.Pacman

goGet :: String -> Recipe ()
goGet repo = withRecipeName "Go.goGet" $ do
    requirePackages ["go"]
    withTempDir $ \tmpDir -> do
        withEnv [("GOPATH", tmpDir)] $ do
            runProc "go" ["get", repo]
            runProc "cp" [tmpDir </> "bin/*", "/usr/local/bin"]
