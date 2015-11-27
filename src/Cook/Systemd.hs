module Cook.Systemd (
    enableService
  ) where

import Cook.Recipe

enableService :: String -> Step
enableService srv = proc "systemd" $ ["enable"] ++ [srv ++ ".service"]
