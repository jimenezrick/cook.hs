import Cook.Recipe
import Cook.Catalog.Cjdns

main :: IO ()
main = do
    testCjdns
    testSsh
    testError

testCjdns :: IO ()
testCjdns = runRecipe $ do
    requireCjdns "conf/cjdns/node.yaml"

testSsh :: IO ()
testSsh = runRecipe $ do
    withSshUser "root" "51.15.58.63" $ do
        runProc0 "uptime"

testError :: IO ()
testError = runRecipe $ do
    withRecipeName "foo.bar.xxx.in" $ do
        withRecipeName "foo.bar.here" $ do
            withRecipeName "foo.bar.yyy.there" $ do
                failWith "WTF"
