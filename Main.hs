{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

import Control.Monad.IO.Class
import System.Process

import qualified Data.Text.Lazy.IO as T

import Control.Monad
import Data.Data
import Data.Tagged
import Data.Yaml
import GHC.Generics

import Deploy

data Ctx = Ctx { foo :: String
               , bar :: Int
               , bur :: Maybe Int
               } deriving (Show, Data, Typeable, Generic)

instance FromJSON Ctx

main :: IO ()
main = void $ runStepM $ do
    liftIO $ putStrLn "Start"

    --run $ cmd_ "false"
    p <- run $ inBackground $ cmd "sleep" ["2"]

    out <- run $ withOutText $ cmd_ "date"
    liftIO $ T.putStr out

    run $ cmd "uname" ["-a"]
    run $ sh "dmesg" .| sh "uniq" .| sh "wc"

    waitFinished p

    liftIO $ putStrLn "End"
