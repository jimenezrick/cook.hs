import Cook.Recipe

main :: IO ()
main = do
    conf <- defRecipeConf
    runRecipe conf $ do
        withCd "/tmp" $ run $ proc "ssh" ["buyvm.untroubled.be", "uname -a"]
