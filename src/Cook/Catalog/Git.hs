module Cook.Catalog.Git (
    gitClone
  ) where

import Cook.Recipe

gitClone :: String -> Recipe f ()
gitClone repo = withRecipeName "Git.Clone" $ runProc "git" ["clone", repo]
