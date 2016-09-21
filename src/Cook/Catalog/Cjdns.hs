module Cook.Catalog.Cjdns (
    CjdnsOpts (..)
  , requireCjdns
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text)
import Data.Maybe
import GHC.Generics
import System.FilePath.Find

import Cook.Recipe
import Cook.Recipe.Config
import Cook.Recipe.Util
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd

--
-- TODO: create passwords, nftables
--
-- Create ad-hoc recipe to generate random passwords
--

data CjdnsOpts = CjdnsOpts
    { privateKey          :: Text
    , publicKey           :: Text
    , ipv6                :: Text
    , authorizedPasswords :: Array
    , udpInterface        :: Array
    } deriving (Show, Generic)

instance ToJSON CjdnsOpts

instance FromJSON CjdnsOpts

requireCjdns :: FilePath -> Recipe ()
requireCjdns optsPath = withRecipeName "Cjdns.RequireCjdns" $ do
    opts <- loadConfig YAML optsPath
    setUpCjdns opts

setUpCjdns :: CjdnsOpts -> Recipe ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    {-
     -requirePackages ["cjdns", "curl"]
     -createFsTree "/etc" $ File "cjdroute.conf" (Template "cjdns" confTemplate opts) (Just 0o600, Just ("root", "root"))
     -enableService "cjdns"
     -startService "cjdns"
     -}

    defConf <- generateConfig
    peers <- getPeers
    conf <- writeConfig JSON $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 0 . key "connectTo") peers $
            mergeConfig (toJSON opts) defConf
    createFsTree "/tmp" $ File "cjdroute.conf" (Content conf) defAttrs

getPeers :: Recipe Value
getPeers = withRecipeName "GetPeers" $ do
    withTempDir $ do
        runSh "curl -s -L https://github.com/hyperboria/peers/archive/master.tar.gz | tar xz --strip-components=1"
        peerFiles <- liftIO $ find always (extension ==? ".k") "."

        liftIO $ putStrLn "Loading public peers:"
        liftIO $ forM_ peerFiles putStrLn

        peersList <- catMaybes <$> mapM (loadConfig JSON) peerFiles
        return $ foldl1 mergeConfig peersList

generateConfig :: Recipe Value
generateConfig = withRecipeName "GenerateConfig" $ do
    conf <- runPipeRead [proc "cjdroute" ["--genconf"], proc "cjdroute" ["--cleanconf"]]
    readConfig JSON conf
