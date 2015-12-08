module Cook.Catalog.Systemd.Container (
    makeArchRootFs
  , launchContainer
  ) where

import Control.Monad.IO.Class
import Data.UUID
import Data.UUID.V4
import System.Directory
import System.FilePath

import Cook.Recipe

-- TODO: machinectl pull BTRFS/tar?

makeArchRootFs :: FilePath -> Recipe FilePath
makeArchRootFs path = withRecipeName "Systemd.Container.MakeArchBase" $ do
    uuid <- liftIO nextRandom
    let containerPath = path </> "arch-base-" ++ toString uuid
    liftIO $ createDirectory containerPath
    run $ proc "pacstrap" ["-i", "-c", "-d", containerPath, "--needed", "base", "--ignore", "linux", "--noconfirm"]
    run $ proc "ln" ["-s", "-f", containerPath, path </> "arch-base"]
    return containerPath

launchContainer :: FilePath -> Recipe ()
launchContainer path = run $ proc "systemd-nspawn" ["-b", "-D", path]
