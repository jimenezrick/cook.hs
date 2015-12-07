import Control.Monad.IO.Class
import Data.Text.Lazy

import qualified Data.Text.Lazy.IO as T

import Cook.Recipe

main :: IO ()
main = do
    conf <- defRecipeConf
    runRecipe conf $ withRecipeName "main" $ do
        {-
         -withSudo $ run $ proc' "id"
         -withSudoUser "nobody" $ run $ proc' "id"
         -}
        (o, _) <- foo
        liftIO $ print o
        run $ sh "pwd"

        withCd ".." $ do
            run $ sh "pwd"
            run $ sh "echo $FOO"

        (o3, _) <- runRead $ proc "echo" ["hello", "$USER"]
        liftIO $ T.putStr o3
        runRead $ proc "echo" ["exit"]

foo :: Recipe (Text, Text)
foo = withRecipeName "foo" $ withCd "/tmp" $ do
    withRecipeName "caca" $ withoutError $ run $ sh "cat caca"
    conf <- recipeConf
    run $ proc "echo" ["hostname:", recipeConfHostName conf]
    run $ proc' "pwd"
    run $ proc' "true"
    run $ sh "echo $USER"
    runRead $ sh "echo xxx"
