import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Text.Printf

import Cook.Recipe
import Cook.Catalog.Arch.Rootfs
import Cook.Catalog.Systemd.Container

main :: IO ()
main = runRecipe $ do
    args <- liftIO $ getArgs
    case args of
        "create-container":name:extraPkgs -> do
            void $ buildRootfs name extraPkgs
            createEmbeddedCookDir name
            liftIO $ printf "Rootfs created: %s\n" name
        ["tar-container", name] -> do
            tarball <- tarRootfs name
            liftIO $ printf "Tarball created: %s\n" tarball
        _ -> error "Invalid arguments"
