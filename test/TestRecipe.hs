import Control.Monad.IO.Class
import Data.Text.Lazy

import qualified Data.Text.Lazy.IO as T

import Cook.Recipe

main :: IO ()
main = do
    conf <- defRecipeConf
    runRecipe conf $ do
        (o, _) <- foo
        liftIO $ print o
        run $ sh "pwd"

        withCd ".." $ do
            run $ sh "pwd"
            run $ sh "echo $USER2"

        (o3, _) <- runRead $ proc "echo" ["hello", "$USER"]
        liftIO $ T.putStr o3
        runRead $ proc "echo" ["exit"]

foo :: Recipe (Text, Text)
foo = withCd "/" $ do
    run $ proc' "pwd"
    run $ proc' "true"
    run $ sh "echo $USER"
    runRead $ sh "echo xxx"
