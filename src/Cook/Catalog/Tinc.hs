module Cook.Catalog.Tinc (
    tinc
  ) where

import Data.Data
import Data.Yaml
import GHC.Generics

import Cook.Recipe
import Cook.Catalog.Pacman
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
    copyConfig
    installPackages ["tinc"]
    enableService "tinc"

copyConfig :: Recipe ()
copyConfig = withRecipeName "Config" $ do
    -- TODO: Allow to pass default values to the asTemplate
    conf <- defTincConf

    createFsTree "/tmp/etc" $
        Dir "tinc" defAttrs [
            File "tinc-up" undefined defAttrs
          , File "tinc-down" undefined defAttrs
          , File "tinc-net.conf" (Copy "tinc_net.conf") defAttrs
          , File "tinc.conf" (Template (asTemplate "tinc.conf.mustache" :: TemplateConf TincConf)) defAttrs
          ]
