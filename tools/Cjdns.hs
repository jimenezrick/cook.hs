import Control.Monad.IO.Class
import System.Environment

import Cook.Recipe
import Cook.Catalog.Systemd.Container
import Cook.Catalog.Cjdns

main :: IO ()
main = runRecipe $ do
    args <- liftIO $ getArgs
    case args of
        ["prepare-container", name, cjdnsToolPath, nodeConfPath] -> do
            embedFsTree name $
                File "bin/cjdns" (Copy cjdnsToolPath) defAttrs
            embedFsTree name $ Dir "conf/cjdns" defAttrs
                [File "node.yaml" (Copy nodeConfPath) (Just 0o600, Just ("root", "root"))]
        _ -> requireCjdns "/cook/conf/cjdns/node.yaml"
