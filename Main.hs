import Cook.Recipe

main :: IO ()
main = runRecipe $ do
    withCd "/tmp" $ run $ proc "ssh" ["buyvm.untroubled.be"]
