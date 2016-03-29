module Cook.Catalog.Systemd (
    enableService
  , disableService
  , setHostname
  ) where

import Text.Printf

import Cook.Recipe

enableService :: String -> Recipe ()
enableService name = withRecipeName "Systemd.EnableService" $ runProc "systemctl" ["enable", unit]
  where unit = printf "%s.service" name

disableService :: String -> Recipe ()
disableService name = withRecipeName "Systemd.DisableService" $ runProc "systemctl" ["disable", unit]
  where unit = printf "%s.service" name

setHostname :: String -> Recipe ()
setHostname hostname = withRecipeName "Systemd.SetHostname" $ runProc "hostnamectl" ["set-hostname", hostname]
