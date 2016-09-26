import Cook.Recipe
import Cook.Catalog.Cjdns

main :: IO ()
main = runRecipe $ do
    requireCjdns "/cook/conf/cjdns/node.yaml"
