module Cook.Catalog.Systemd (
    enableService
  ) where

import Cook.Recipe

enableService :: String -> Recipe ()
enableService srv = withRecipeName "Systemd" $ run $ proc "systemd" $ "enable":[srv ++ ".service"]
