module Cook.Catalog.Cjdns (
    requireCjdns
  ) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.FileEmbed
import Data.Text (Text)
import GHC.Generics

import Cook.Recipe
import Cook.Recipe.Config
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd

--
-- TODO: Add peers, create passwords, nftables
--

data CjdnsOpts = CjdnsOpts
    { _privateKey :: Text
    , _publicKey  :: Text
    , _ipv6       :: Text
    } deriving Generic

instance ToJSON CjdnsOpts

instance FromJSON CjdnsOpts where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

confTemplate :: ByteString
confTemplate = $(embedFile "ingredients/cjdns/cjdroute.conf.mustache")

requireCjdns :: FilePath -> Recipe ()
requireCjdns confPath = withRecipeName "Cjdns.RequireCjdns" $ do
    conf <- loadConfig confPath
    setUpCjdns conf

setUpCjdns :: CjdnsOpts -> Recipe ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    requirePackages ["cjdns"]
    createFsTree "/etc" $ File "cjdroute.conf" (Template "cjdns" confTemplate opts) (Just 0o600, Just ("root", "root"))
    enableService "cjdns"
    startService "cjdns"
