module Cook.Recipe.Bootstrap (
    bootstrapCook
  ) where

import Cook.Recipe
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Git

bootstrapCook :: Recipe ()
bootstrapCook = withRecipeName "Recipe.BootstrapCook" $ do
    requirePackages ["ghc", "cabal-install"]
    gitClone "https://github.com/jimenezrick/cook.hs.git"
    withCd "cook" $ do
        runProc "cabal" ["install", "exe:cook"]
