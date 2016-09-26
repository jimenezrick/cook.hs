import Control.Monad
import Control.Monad.IO.Class
import System.Environment
import Text.Printf

import Cook.Recipe
import Cook.Recipe.Bootstrap
import Cook.Catalog.Arch.Rootfs
import Cook.Catalog.Systemd.Container
import Cook.Catalog.Cjdns

main :: IO ()
main = runRecipe $ do
    args <- liftIO $ getArgs
    case args of
        "create-container":name:extraPkgs -> do
            void $ buildRootfs name extraPkgs
            createEmbeddedCookDir name
            embedCookBin name
            liftIO $ printf "Rootfs created: %s\n" name
        "tar-container":name:embedFiles -> do
            -- TODO: embedFiles path:cook_path

            embedInCookDir undefined -- XXX

            tarball <- tarRootfs name
            liftIO $ printf "Tarball created: %s\n" tarball
        "build-container":name:embedFiles -> do
            -- machinectl start, shell, poweroff
        _ -> error "Invalid arguments"
