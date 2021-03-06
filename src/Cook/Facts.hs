module Cook.Facts
    ( Distro (..)
    , Facts
    , distro
    , release
    , osRelease
    , currentTime
    , systemFacts
    , customFacts

    , grabFacts
    , grabOnlySystemFacts
    , grabSystemFacts
    , grabOsRelease
    ) where

import Control.Exception.Safe (throwString)
import Control.Lens
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Maybe (mapMaybe)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Distro = Arch | Debian | Ubuntu deriving Show

lookupDistro :: Text -> Maybe Distro
lookupDistro name = M.lookup name distros
  where distros = [ ("arch", Arch)
                  , ("debian", Debian)
                  , ("ubuntu", Ubuntu)
                  ]

data OsRelease = OsRelease
    { _distro  :: Distro
    , _release :: Maybe Text
    } deriving Show

data SystemFacts = SystemFacts
    { _osRelease   :: OsRelease
    , _currentTime :: UTCTime
    } deriving Show

makeLenses ''OsRelease
makeLenses ''SystemFacts

data Facts a where
    Facts :: Show a =>
        { _systemFacts :: SystemFacts
        , _customFacts :: a
        } -> Facts a

deriving instance Show (Facts a)

-- Note: can't generate lens for GADTs
systemFacts :: Lens' (Facts f) SystemFacts
systemFacts g (Facts system custom) = fmap (\system' -> Facts system' custom) (g system)

customFacts :: Lens' (Facts f) f
customFacts g (Facts system custom) = fmap (\custom' -> Facts system custom') (g custom)

grabFacts :: Show a => a -> IO (Facts a)
grabFacts cusFacts = Facts <$> grabSystemFacts <*> return cusFacts

grabOnlySystemFacts :: IO (Facts ())
grabOnlySystemFacts = grabFacts ()

grabSystemFacts :: IO SystemFacts
grabSystemFacts = SystemFacts <$> grabOsRelease <*> getCurrentTime

grabOsRelease :: IO OsRelease
grabOsRelease = do
    info <- T.readFile "/etc/os-release"
    let kv = M.fromList . mapMaybe (takeKV . T.splitOn "=") . drop 1 $ T.lines info
        os = do
            let rel = T.dropAround (== '"') <$> M.lookup "VERSION_ID" kv
            dis <- M.lookup "ID" kv >>= lookupDistro
            return (dis, rel)
    case os of
        Just (dis, rel) -> return $ OsRelease dis rel
        Nothing -> throwString "Couldn't parse facts from: /etc/os-release"
  where takeKV [k, v] = Just (T.dropWhileEnd (== ':') k, v)
        takeKV _      = Nothing
