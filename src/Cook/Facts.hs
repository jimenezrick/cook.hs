module Cook.Facts
    ( grabSystemFacts
    , grabOsRelease
    ) where

import Data.Maybe
import Data.Text (Text)

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Cook.Recipe
import Cook.Recipe.Util

-- TODO:
-- 1. Insert in Recipe Ctx?
-- 2. How to let the user to expand it?

data Distro = Arch | Debian | Ubuntu | CentOS

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
    }

data Facts = Facts
    { _osRelease :: OsRelease
    }

grabSystemFacts :: Recipe Facts
grabSystemFacts = withRecipeName "Facts.GrabFacts" $
    Facts <$> grabOsRelease

grabOsRelease :: Recipe OsRelease
grabOsRelease = withRecipeName "Facts.GrabOsRelease" $ do
    info <- withFileContent "/etc/os-release"
    let kv = M.fromList . mapMaybe (takeKV . T.splitOn "\n") . drop 1 $ T.lines info
        os = do
            let release = T.dropAround (== '"') <$> M.lookup "VERSION_ID" kv
            distro <- M.lookup "ID" kv >>= lookupDistro
            return (distro, release)
    case os of
        Just (distro, release) -> return $ OsRelease distro release
        Nothing -> failWith "Couldn't parse facts from: /etc/os-release"
  where takeKV [k, v] = Just (T.dropWhileEnd (== ':') k, v)
        takeKV _      = Nothing
