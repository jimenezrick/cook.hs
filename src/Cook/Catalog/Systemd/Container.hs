module Cook.Catalog.Systemd.Container (
    makeArchBase
  ) where

import Control.Monad.IO.Class
import Data.UUID
import Data.UUID.V4
import System.Directory
import System.FilePath

import Cook.Recipe

makeArchBase :: FilePath -> Recipe FilePath
makeArchBase path = withRecipeName "Systemd.Container.MakeArchBase" $ do
    uuid <- liftIO $ nextRandom
    let containerPath = path </> "arch-base-" ++ toString uuid
    liftIO $ createDirectory containerPath
    run $ proc "pacstrap" ["-i", "-c", "-d", containerPath, "--needed", "base", "--ignore", "linux", "--noconfirm"]
    return containerPath
