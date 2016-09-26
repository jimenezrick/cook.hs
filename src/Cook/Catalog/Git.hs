module Cook.Catalog.Git (
    gitClone
  ) where

import Cook.Recipe

gitClone :: String -> Recipe ()
gitClone repo = withRecipeName "Git.Clone" $ runProc "git" ["clone", repo]
