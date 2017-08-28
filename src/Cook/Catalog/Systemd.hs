module Cook.Catalog.Systemd (
    enableService
  , disableService
  , startService
  , stopService
  , setHostname
  ) where

import Text.Printf

import Cook.Recipe

enableService :: String -> Recipe f ()
enableService name = withRecipeName "Systemd.EnableService" $ runProc "systemctl" ["enable", unit]
  where unit = printf "%s.service" name

disableService :: String -> Recipe f ()
disableService name = withRecipeName "Systemd.DisableService" $ runProc "systemctl" ["disable", unit]
  where unit = printf "%s.service" name

startService :: String -> Recipe f ()
startService name = withRecipeName "Systemd.StartService" $ runProc "systemctl" ["start", unit]
  where unit = printf "%s.service" name

stopService :: String -> Recipe f ()
stopService name = withRecipeName "Systemd.StopService" $ runProc "systemctl" ["stop", unit]
  where unit = printf "%s.service" name

setHostname :: String -> Recipe f ()
setHostname hostname = withRecipeName "Systemd.SetHostname" $ runProc "hostnamectl" ["set-hostname", hostname]
