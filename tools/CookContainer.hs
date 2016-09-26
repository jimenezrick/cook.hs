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
        [container] -> do
            liftIO $ printf "Building Arch rootfs...\n"
            void $ buildRootfs container

            liftIO $ printf "Compressing container...\n"
            tarball <- compressRootfs container
            liftIO $ printf "Done: %s\n" tarball
        _ -> error "Invalid args"
