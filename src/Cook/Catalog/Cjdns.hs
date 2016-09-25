module Cook.Catalog.Cjdns (
    CjdnsOpts (..)
  , requireCjdns
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.Text.Lazy (Text)
import Data.Maybe
import GHC.Generics
import System.FilePath.Find

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
    peers <- getPeers
    conf <- writeConfig JSON $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 0 . key "connectTo") peers $
            mergeConfig (toJSON opts) defConf

    createFsTree "/etc" $ File "cjdroute.conf" (Content conf) (Just 0o600, Just ("root", "root"))
    enableService "cjdns"
    startService "cjdns"

getPeers :: Recipe Value
getPeers = withRecipeName "GetPeers" $ do
    withTempDir $ \tmpDir -> do
        tarball <- getHTTP "https://github.com/hyperboria/peers/archive/master.tar.gz"
        void $ runTakeRead' (proc "tar" ["xz", "--strip-components=1"]) tarball
        peerFiles <- liftIO $ find always (extension ==? ".k") tmpDir

        liftIO $ putStrLn "Loading public peers:"
        liftIO $ forM_ peerFiles putStrLn

        peersList <- catMaybes <$> mapM (loadConfig JSON) peerFiles
        return $ foldl1 mergeConfig peersList

generateConfig :: Recipe Value
generateConfig = withRecipeName "GenerateConfig" $ do
    conf <- runPipeRead [proc "cjdroute" ["--genconf"], proc "cjdroute" ["--cleanconf"]]
    readConfig JSON conf
