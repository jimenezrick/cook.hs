{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables      #-}

import Control.Monad.IO.Class
import System.Process

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
    {-runStep' cmd-}
    {-runStep' tmpl-}



    {-t <- runReadOut echo1-}
    {-run sleep-}
    {-run echo2-}
    {-liftIO $ print t-}

    run $ sh "echo 1" -|- sh "wc" -|- sh "wc"
    run $ sh "yes 2" -|- sh "wc"




    {-(_, _, _, Just hdl) <- runStep yes1-}
    {-liftIO $ waitForProcess hdl-}
    {-runStep yes2-}

    {-runStep' echo $| runStep' wc-}

