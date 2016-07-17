import Control.Monad.IO.Class
import Data.Text.Lazy

import qualified Data.Text.Lazy.IO as T

import Cook.Recipe
import Cook.Catalog.Systemd.Container
import Cook.Catalog.Cjdns

main :: IO ()
main = testCjdns

main' :: IO ()
main' = do
    conf <- defRecipeConf
    runRecipe conf $ withRecipeName "main" $ do
        {-
         -withSudo $ runProc' "id"
         -withSudoUser "nobody" $ runProc' "id"
         -}
        (o, _) <- foo
        liftIO $ print o
        runSh "pwd"

        withCd ".." $ do
            runSh "pwd"
            runSh "echo $FOO"

        (o3, _) <- runRead $ proc "echo" ["hello", "$USER"]
        liftIO $ T.putStr o3
        runRead $ proc "echo" ["exit"]

foo :: Recipe (Text, Text)
foo = withRecipeName "foo" $ withCd "/tmp" $ do
    withRecipeName "caca" $ withoutError $ runSh "cat caca"
    conf <- recipeConf
    runProc "echo" ["hostname:", recipeConfHostName conf]
    runProc' "pwd"
    runProc' "true"
    runSh "echo $USER"
    runRead $ sh "echo xxx"

testContainer :: IO ()
testContainer = do
    conf <- defRecipeConf
    runRecipe conf $ do
        createFsTree "/tmp" $ DirEmpty "CONTAINER" defAttrs
        withSudo $ do
            path <- makeArchLinuxRootFs "/tmp/CONTAINER"
            compressContainerFs path
            --launchContainer path
            return ()

testCjdns :: IO ()
testCjdns = do
    conf <- defRecipeConf
    runRecipe conf $ do
        requireCjdns "cjdroute.conf.yaml"
