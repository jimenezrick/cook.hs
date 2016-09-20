{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Text.Lazy

import qualified Data.Text.Lazy.IO as T

import Cook.Recipe
import Cook.Recipe.Config
import Cook.Catalog.Systemd.Container
import Cook.Catalog.Cjdns

main :: IO ()
main = testConfig

main' :: IO ()
main' = do
    runRecipe $ withRecipeName "main" $ do
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
    runRecipe $ do
        createFsTree "/tmp" $ DirEmpty "CONTAINER" defAttrs
        withSudo $ do
            path <- makeArchLinuxRootFs "/tmp/CONTAINER"
            compressContainerFs path
            --launchContainer path
            return ()

testCjdns :: IO ()
testCjdns = do
    runRecipe $ do
        requireCjdns "cjdroute.conf.yaml"

testEnv :: IO ()
testEnv = do
    runRecipe $ do
        withEnv [("A", "666"), ("USER", "foo")] $ do
            runSh "echo $HOME"
            runSh "echo $USER"
            runSh "echo $A"
        runSh "echo $USER"

testConfig :: IO ()
testConfig = do
    runRecipe $ do
        a <- loadConfig "/tmp/a.json"
        conf <- loadConfig "/tmp/c.json"
        writeConfig "/tmp/out.json" (insertConfigWithKey ["logging", "foo"] a conf)
        writeConfig "/tmp/out.json" (mergeConfig a conf)
