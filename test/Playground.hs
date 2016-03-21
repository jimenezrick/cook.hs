import Cook.Recipe

main :: IO ()
main = do
    conf <- defRecipeConf
    let conf' = conf { recipeConfDebug = False, recipeConfVerbose = True }
    runRecipe conf' $ withRecipeName "Main" $ do
        foo

foo :: Recipe ()
foo = withRecipeName "Foo" $ do
    runProc "true" ["1"]
    runProc "true" ["2"]
    runProc "true" ["3"]
    runProc "true" ["4"]
    runProc "true" ["5"]
    bar

bar :: Recipe ()
bar = withRecipeName "Bar" $ do
    runProc' "true"
