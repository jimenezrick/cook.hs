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
    yes1 -|- wc
    yes2 -|- wc




    {-(_, _, _, Just hdl) <- runStep yes1-}
    {-liftIO $ waitForProcess hdl-}
    {-runStep yes2-}

    {-runStep' echo $| runStep' wc-}

  -- TODO: create aux functions to run and build values
  where --tmpl = Templ' "p.mustache" "p.out" :: Step' Ctx
        --cmd  = Cmd' "echo Hello" :: Step' ()
        --echo = Cmd' "echo Hello" :: Step' ()
        --wc   = Cmd' "wc" :: Step' ()
        yes1   = Sh "yes 1" :: Step' ()
        yes2   = Sh "yes 2" :: Step' ()

        sleep = Sh "sleep 2" :: Step' ()
        echo1 = Sh "echo 1" :: Step' ()
        echo2 = Sh "echo 2" :: Step' ()

        wc = Sh "wc" :: Step' ()
