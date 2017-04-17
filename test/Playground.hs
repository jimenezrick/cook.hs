import Cook.Facts
import Cook.Recipe
import Cook.Recipe.Util

main :: IO ()
main = do
    testSsh
    {-testError-}

testError :: IO ()
testError = runRecipe $ do
    runProc0 "uptime"
    runProc0 "true"
    --getHTTP "foo"

    withRecipeName "foo.bar.xxx.in" $
        withRecipeName "foo.bar.here" $
            withRecipeName "foo.bar.yyy.there" $
                failWith "WTF"

testSsh :: IO ()
testSsh = runRecipe $
    withSsh "alarm" "192.168.1.155" $ do
        runProc0 "uname -a"
        runProc0 "date"
        runProc0 "uptime"
        withCd "/" $ do
            runProc "echo" ["begin"]
            execCwd $ proc0 "ls"
            runProc0 "ls"
            runSh "ls /foo"
            runProc "echo" ["end"]
