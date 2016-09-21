module Cook.Recipe.Util (
    withTempDir
  ) where

import Data.Text.Lazy (unpack)

import Cook.Recipe

withTempDir :: Recipe a -> Recipe a
withTempDir recipe = do
    (tmpDir, _) <- runRead $ proc "mktemp" ["-d", "cook-XXXXXX"]
    let tmpDir' = unpack tmpDir
    a <- withCd tmpDir' recipe
    runProc "rm" ["-rf", tmpDir']
    return a
