module Cook.Catalog.Tinc (
    tinc
  ) where

import Cook.Recipe
import Cook.Catalog.Pacman
import Cook.Catalog.Systemd

tinc :: Recipe ()
tinc = do
    copyConfig
    installPackages ["tinc"]
    enableService "tinc"

copyConfig :: Recipe ()
copyConfig = do
    createFsTree "/tmp/etc" $
        Dir "tinc" defAttrs [
            File "tinc-up" undefined defAttrs
          , File "tinc-down" undefined defAttrs
          , File "tinc_net.conf" (Copy "tinc_net.conf") defAttrs
          ]
