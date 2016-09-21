module Cook.Catalog.Cjdns (
    CjdnsOpts (..)
  , requireCjdns
  ) where

import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Text (Text)
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

-- XXX: Validae this? not used any more
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
requireCjdns confPath = withRecipeName "Cjdns.RequireCjdns" $ do
    conf <- loadConfig JSON confPath
    setUpCjdns conf

setUpCjdns :: CjdnsOpts -> Recipe ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    {-
     -requirePackages ["cjdns", "curl"]
     -createFsTree "/etc" $ File "cjdroute.conf" (Template "cjdns" confTemplate opts) (Just 0o600, Just ("root", "root"))
     -enableService "cjdns"
     -startService "cjdns"
     -}

    conf <- generateConfig
    nodeConf <- loadConfig YAML "conf/cjdns/node.yaml"
    peers <- getPeers

    let conf'  = mergeConfig nodeConf conf
        conf'' = insertConfigInto (key "interfaces" . key "UDPInterface" . nth 0 . key "connectTo") peers conf

    conf''' <- writeConfig JSON conf''

    createFsTree "/tmp" $ File "cjdroute.conf" (Content conf''') defAttrs

getPeers :: Recipe Value
getPeers = withRecipeName "GetPeers" $ do
    peers <- withTempDir $ do
        runSh "curl -s -L https://github.com/hyperboria/peers/archive/master.tar.gz | tar xz"
        peerFiles <- liftIO $ find always (extension ==? ".k") "peers-master"

        liftIO $ putStrLn "Loading peers:"
        liftIO $ forM_ peerFiles putStrLn

        peersList <- catMaybes <$> mapM (loadConfig JSON) peerFiles
        writeConfig JSON $ foldl1 mergeConfig peersList

    liftIO $ print peers

    {-createFsTree "." $ File "peers.json" conf defAttrs-}
    -- inject config



generateConfig :: Recipe Value
generateConfig = do
    conf <- runPipeRead [proc "cjdroute" ["--genconf"], proc "cjdroute" ["--cleanconf"]]
    return $ readConfig conf
