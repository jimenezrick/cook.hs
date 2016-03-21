module Cook.Catalog.Git (
    clone
  ) where

import Cook.Recipe

clone :: String -> Recipe ()
clone repo = withRecipeName "Git.Clone" $ runProc "git" ["clone", repo]
