import Cook.Facts
import Cook.Recipe
import Cook.Recipe.Util
import Cook.Catalog.Cjdns

main :: IO ()
main = do
    {-testSsh-}
    testError

testCjdns :: IO ()
testCjdns = runRecipe $
    requireCjdns "conf/cjdns/node.yaml"

testSsh :: IO ()
testSsh = runRecipe $
    withSshUser "root" "51.15.58.63" $
        runProc0 "uptime"

testError :: IO ()
testError = runRecipe $ do
    runProc0 "uptime"
    runProc0 "true"
    --getHTTP "foo"

    withRecipeName "foo.bar.xxx.in" $
        withRecipeName "foo.bar.here" $
            withRecipeName "foo.bar.yyy.there" $
                failWith "WTF"
