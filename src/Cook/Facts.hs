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

import Control.Error
import Control.Lens
import Control.Monad.Except (throwError)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

--
-- TODO: Add support for custom facts in SSH execution mode
--

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

grabFacts :: Show a => IO a -> IO (Facts a)
grabFacts grabCustom = runScript $ Facts <$> grabSystemFacts <*> scriptIO grabCustom

grabOnlySystemFacts :: IO (Facts ())
grabOnlySystemFacts = grabFacts $ return ()

grabSystemFacts :: Script SystemFacts
grabSystemFacts = SystemFacts <$> grabOsRelease <*> scriptIO getCurrentTime

grabOsRelease :: Script OsRelease
grabOsRelease = do
    info <- scriptIO $ T.readFile "/etc/os-release"
    let kv = M.fromList . mapMaybe (takeKV . T.splitOn "=") . drop 1 $ T.lines info
        os = do
            let rel = T.dropAround (== '"') <$> M.lookup "VERSION_ID" kv
            dis <- M.lookup "ID" kv >>= lookupDistro
            return (dis, rel)
    case os of
        Just (dis, rel) -> return $ OsRelease dis rel
        Nothing -> throwError "Couldn't parse facts from: /etc/os-release"
  where takeKV [k, v] = Just (T.dropWhileEnd (== ':') k, v)
        takeKV _      = Nothing
