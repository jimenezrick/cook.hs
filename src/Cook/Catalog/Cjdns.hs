module Cook.Catalog.Cjdns (
    requireCjdns
  , requireCjdnsWith
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics

import qualified Data.ByteString.Lazy as B

import Cook.Recipe
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

instance FromJSON CjdnsOpts where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

makeLenses ''CjdnsOpts

requireCjdns :: Recipe ()
requireCjdns = withRecipeName "Cjdns.RequireCjdns" $ do
    setUpCjdns Nothing

requireCjdnsWith :: FilePath -> Recipe ()
requireCjdnsWith confPath = withRecipeName "Cjdns.RequireCjdnsWith" $ do
    conf <- liftIO $ B.readFile confPath -- TODO: Catch IO exception, write function that cathes error
    case eitherDecode conf of
        Left err   -> failWith err
        Right opts -> setUpCjdns $ Just opts

setUpCjdns :: Maybe CjdnsOpts -> Recipe ()
setUpCjdns opts = withRecipeName "SetUpCjdns" $ do
    requirePackages ["cjdns"]
    conf <- maybe generateConfig generateConfigWith opts
    let prettyConf = decodeUtf8 . B.toStrict $ flip encodePretty' conf defConfig
            { confCompare = keyOrder
                    [ "privateKey"
                    , "publicKey"
                    , "ipv6"
                    , "admin"
                    , "authorizedPasswords"
                    , "router"
                    , "interfaces"
                    ]
            }
    createFsTree "/etc" $ File "cjdroute.conf" (Content prettyConf) (Just 0o600, Just ("root", "root"))
    enableService "cjdns"
    startService "cjdns"

generateConfigWith :: CjdnsOpts -> Recipe Object
generateConfigWith opts = withRecipeName "GenerateConfigWith" $ do
    obj <- generateConfig
    return $ obj & ix "privateKey" .~ (String $ opts^.privateKey)
                 & ix "publicKey" .~ (String $ opts^.publicKey)
                 & ix "ipv6" .~ (String $ opts^.ipv6)

generateConfig :: Recipe Object
generateConfig = withRecipeName "GenerateConfig" $ do
    conf <- runPipeRead [proc "cjdroute" ["--genconf", "--no-eth"], proc "cjdroute" ["--cleanconf"]]
    case eitherDecode (B.fromStrict . encodeUtf8 $ toStrict conf) of
        Left err  -> failWith err
        Right obj -> return obj
