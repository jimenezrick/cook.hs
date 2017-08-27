module Cook.Facts
    ( Facts
    , grabSystemFacts
    , grabOsRelease
    ) where

import Control.Error
import Control.Monad.Except
import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- TODO:
-- * Insert in Recipe Ctx?
-- * How to let the user to expand it?

data Distro = Arch | Debian | Ubuntu | CentOS deriving Show

lookupDistro :: Text -> Maybe Distro
lookupDistro name = M.lookup name distros
  where distros = [ ("arch", Arch)
                  , ("debian", Debian)
                  , ("ubuntu", Ubuntu)
                  , ("centos", CentOS)
                  ]

data OsRelease = OsRelease
    { _distro  :: Distro
    , _release :: Maybe Text
    } deriving Show

data Facts = Facts
    { _osRelease :: OsRelease
    } deriving Show

grabSystemFacts :: IO Facts
grabSystemFacts = runScript $ Facts <$> grabOsRelease

grabOsRelease :: Script OsRelease
grabOsRelease = do
    info <- scriptIO $ T.readFile "/etc/os-release"
    let kv = M.fromList . mapMaybe (takeKV . T.splitOn "=") . drop 1 $ T.lines info
        os = do
            let release = T.dropAround (== '"') <$> M.lookup "VERSION_ID" kv
            distro <- M.lookup "ID" kv >>= lookupDistro
            return (distro, release)
    case os of
        Just (distro, release) -> return $ OsRelease distro release
        Nothing -> throwError "Couldn't parse facts from: /etc/os-release"
  where takeKV [k, v] = Just (T.dropWhileEnd (== ':') k, v)
        takeKV _      = Nothing
