module Cook.Systemd (
    enableService
  ) where

import Cook.Recipe

enableService :: String -> Recipe ()
enableService srv = run $ proc "systemd" $ ["enable"] ++ [srv ++ ".service"]
