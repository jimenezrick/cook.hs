import Cook.Recipe
import Cook.Pacman

main :: IO ()
main = runRecipe $ do
    run $ installPackages ["tinc"]
