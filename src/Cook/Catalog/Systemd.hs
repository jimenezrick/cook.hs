module Cook.Catalog.Systemd (
    enableService
  ) where

import Text.Printf

import Cook.Recipe

type ServiceName = String

enableService :: ServiceName -> Recipe ()
enableService name = withRecipeName "Systemd.EnableService" $ runProc "systemctl" ["enable", unit]
  where unit = printf "%s.service" name
