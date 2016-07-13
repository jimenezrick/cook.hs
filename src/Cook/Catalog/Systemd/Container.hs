module Cook.Catalog.Systemd.Container (
    makeArchLinuxRootFs
  , compressContainerFs
  , launchContainer
  ) where

import Control.Monad.IO.Class
import Data.List
import Data.UUID
import Data.UUID.V4
import System.Directory
import System.FilePath

import qualified Data.Text.Lazy as T

import Cook.Recipe

-- TODO: Maybe send manually BTRFS snapshot and not using systemd-importd
-- XXX: Use cpio instead?

makeArchLinuxRootFs :: FilePath -> Recipe FilePath
makeArchLinuxRootFs path = withRecipeName "Systemd.Container.MakeContainerArchRootFs" $ do
    uuid <- liftIO nextRandom
    let containerPath = path </> "arch-base-" ++ toString uuid
    liftIO $ createDirectory containerPath
    (base, _) <- runRead $ proc "pacman" ["--quiet", "-Q", "--groups", "base"]
    run $ proc "pacstrap" $ ["-i", "-c", "-d", containerPath, "--needed", "--noconfirm"] ++ withoutLinux base
    run $ proc "ln" ["-s", "-f", "-T", containerPath, path </> "arch-base"]
    return containerPath
  where withoutLinux = map T.unpack . delete "linux" . T.lines

compressContainerFs :: FilePath -> Recipe FilePath
compressContainerFs path = withRecipeName "Systemd.Container.CompressContainerFs" $ do
    let tarballPath = path ++ ".tar.gz"
    run $ proc "tar" ["cfz", tarballPath, path]
    return tarballPath

launchContainer :: FilePath -> Recipe ()
launchContainer path = withRecipeName "Systemd.Container.LaunchContainer" $ do
    run $ proc "systemd-nspawn" ["-b", "-D", path]
