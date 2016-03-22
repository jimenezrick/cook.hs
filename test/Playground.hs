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
    withoutError bar
    beer

bar :: Recipe ()
bar = withRecipeName "Bar" $ do
    withoutError $ run $ failWith "shit 1"
    run $ failWith "shit 2"
    runProc "echo" ["bar"]

beer :: Recipe ()
beer = withRecipeName "Beer" $ do
    runProc "echo" ["beer"]
