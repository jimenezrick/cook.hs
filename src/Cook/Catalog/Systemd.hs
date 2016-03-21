module Cook.Catalog.Systemd (
    enableService
  ) where

import Text.Printf

import Cook.Recipe

enableService :: String -> Recipe ()
enableService name = withRecipeName "Systemd.EnableService" $ runProc "systemctl" ["enable", unit]
  where unit = printf "%s.service" name
