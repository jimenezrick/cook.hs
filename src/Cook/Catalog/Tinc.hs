module Cook.Catalog.Tinc (
    tinc
  ) where

import Data.Data
import Data.Yaml
import GHC.Generics

import Cook.Recipe
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd

-- TODO: nftables

data TincConf = TincConf {
    tincConfNode :: String
  } deriving (Data, Typeable, Generic)

instance FromJSON TincConf

defTincConf :: Recipe TincConf
defTincConf = do
    conf <- recipeConf
    return TincConf { tincConfNode = shorten $ recipeConfHostName conf }
  where shorten = takeWhile (/= '.')

tinc :: Recipe ()
tinc = withRecipeName "Tinc" $ do
    placeTincConf
    installPackages ["tinc"]
    enableService "tinc"

placeTincConf :: Recipe ()
placeTincConf = withRecipeName "Config" $ do
    tincConf <- defTincConf
    createFsTree "/tmp/etc" $
        Dir "tinc" defAttrs [
            File "tinc-up" undefined defAttrs
          , File "tinc-down" undefined defAttrs
          , File "tinc-net.conf" (Copy "tinc_net.conf") defAttrs
          , File "tinc.conf" (TemplateWithDef tincConf (asTemplate "tinc.conf.mustache" :: TemplateConf TincConf)) defAttrs
          ]
