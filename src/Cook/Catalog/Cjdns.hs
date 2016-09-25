module Cook.Catalog.Cjdns (
    CjdnsOpts (..)
  , requireCjdns
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Lens
import Data.Text.Lazy (Text)
import Data.Maybe
import GHC.Generics
import System.FilePath.Find

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Cook.Recipe
import Cook.Recipe.Config
import Cook.Recipe.Util
import Cook.Catalog.Arch.Pacman
import Cook.Catalog.Systemd

--
-- TODO: nftables
--

data CjdnsOpts = CjdnsOpts
    { privateKey          :: Text
    , publicKey           :: Text
    , ipv6                :: Text
    , authorizedPasswords :: Maybe Array
    , interfaces          :: Object
    , logging             :: Maybe Object
    } deriving (Show, Generic)

instance ToJSON CjdnsOpts

instance FromJSON CjdnsOpts

requireCjdns :: FilePath -> Recipe ()
requireCjdns optsPath = withRecipeName "Cjdns.RequireCjdns" $ do
    requirePackages ["cjdns"]
    opts <- loadConfig YAML optsPath
    setUpCjdns opts

setUpCjdns :: CjdnsOpts -> Recipe ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    defConf <- generateConfig
    (peersIPv4, peersIPv6) <- getPeers
    conf <- writeConfig JSON $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 1 . key "connectTo") peersIPv6 $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 0 . key "connectTo") peersIPv4 $
            mergeConfig (toJSON opts) defConf

    createFsTree "/etc" $ File "cjdroute.conf" (Content conf) (Just 0o600, Just ("root", "root"))
    enableService "cjdns"
    startService "cjdns"
    --createFsTree "." $ File "cjdroute.conf" (Content conf) defAttrs

getPeers :: Recipe (Value, Value)
getPeers = withRecipeName "GetPeers" $ do
    withTempDir $ \tmpDir -> do
        tarball <- getHTTP "https://github.com/hyperboria/peers/archive/master.tar.gz"
        void $ runTakeRead' (proc "tar" ["xz", "--strip-components=1"]) tarball
        peerFiles <- liftIO $ find always (extension ==? ".k") tmpDir

        liftIO $ putStrLn "Loading public peers:"
        liftIO $ forM_ peerFiles putStrLn

        Object peers <- foldl mergeConfig emptyObject . catMaybes <$> mapM (loadConfig JSON) peerFiles
        let peersIPv6 = H.filterWithKey isIPv6 peers
            peersIPv4 = H.difference peers peersIPv6
        return (Object peersIPv4, Object peersIPv6)
  where isIPv6 a _ = T.head a == '['

generateConfig :: Recipe Value
generateConfig = withRecipeName "GenerateConfig" $ do
    conf <- runPipeRead [proc "cjdroute" ["--genconf"], proc "cjdroute" ["--cleanconf"]]
    readConfig JSON conf
