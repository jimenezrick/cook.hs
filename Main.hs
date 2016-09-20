import Cook.Recipe

main :: IO ()
main = do
    runRecipe $ do
        withCd "/tmp" $ run $ proc "ssh" ["buyvm.untroubled.be", "uname -a"]
