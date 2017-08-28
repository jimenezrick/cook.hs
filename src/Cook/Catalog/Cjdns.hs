module Cook.Catalog.Cjdns (
    CjdnsOpts (..)
  , setUpCjdns
  , requireCjdns
  , requireCjdcmd
  ) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.ByteString.Lazy (fromStrict)
import Data.FileEmbed (embedFile)
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Generics
import System.FilePath.Find

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Cook.Provider
import Cook.Recipe
import Cook.Recipe.Config
import Cook.Recipe.Util
import Cook.Catalog.Go

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

requireCjdns :: FilePath -> Recipe f ()
requireCjdns optsPath = withRecipeName "Cjdns.RequireCjdns" $ do
    prov <- getProvider
    prov^.pkgManager.requirePackages $ ["cjdns"]
    opts <- loadConfig YAML optsPath
    setUpCjdns opts

setUpCjdns :: CjdnsOpts -> Recipe f ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    defConf <- generateConfig
    (peersIPv4, peersIPv6) <- getPeers
    conf <- writeConfig JSON $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 1 . key "connectTo") peersIPv6 $
            insertConfigInto (key "interfaces" . key "UDPInterface" . nth 0 . key "connectTo") peersIPv4 $
            mergeConfig (toJSON opts) defConf

    createFsTree "/etc" $ File "cjdroute.conf" (Content conf) (Just 0o600, Just ("root", "root"))

    -- To test conf generation: createFsTree "." $ File "cjdroute.conf" (Content conf) defAttrs

getPeers :: Recipe f (Value, Value)
getPeers = withRecipeName "GetPeers" $
    withTempDir $ \tmpDir -> do
        tarball <- getHTTP "https://github.com/hyperboria/peers/archive/master.tar.gz"
        void $ runInOutB tarball (proc "tar" ["xz", "--strip-components=1"])
        peerFiles <- recipeIO $ find always (extension ==? ".k") tmpDir

        recipeIO $ putStrLn "Loading public peers:"
        recipeIO $ forM_ peerFiles putStrLn

        Object peers <- foldl mergeConfig emptyObject . catMaybes <$> mapM (loadConfig JSON) peerFiles
        let peersIPv6 = H.filterWithKey isIPv6 peers
            peersIPv4 = H.difference peers peersIPv6
        return (Object peersIPv4, Object peersIPv6)
  where isIPv6 a _ = T.head a == '['

generateConfig :: Recipe f Value
generateConfig = withRecipeName "GenerateConfig" $ do
    conf <- runPipeOut [proc "cjdroute" ["--genconf"], proc "cjdroute" ["--cleanconf"]]
    readConfig JSON conf

requireCjdcmd :: Recipe f ()
requireCjdcmd = withRecipeName "Cjdns.RequireCjdcmd" $ do
    goGet "github.com/fc00/cjdcmd-ng"
    createFsTree "/root" $ File ".cjdnsadmin" (Content cjdcmdConf) defAttrs

cjdcmdConf :: Text
cjdcmdConf = decodeUtf8 $ fromStrict $(embedFile "conf/cjdns/cjdnsadmin")
