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
            void $ buildRootfs container
            tarball <- compressRootfs container
            liftIO $ printf "Container created: %s\n" tarball
        _ -> error "Invalid args"
