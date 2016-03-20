module Cook.Catalog.Git (
    clone
  ) where

import Cook.Recipe

type Repo = String

clone :: Repo -> Recipe ()
clone repo = withRecipeName "Git.Clone" $ runProc "git" ["clone", repo]
