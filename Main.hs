{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE ScopedTypeVariables      #-}

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

{-
 -main :: IO ()
 -main = useTemplate (Tagged "p.mustache" :: Template Ctx) "p.out"
 -}

{-
 -main :: IO ()
 -main = runStep step
 -  where step = Templ "p.mustache" "p.out" :: Step Ctx
 -}



main :: IO ()
main = runStepM $ do
    runStep' cmd
    runStep' tmpl
  where tmpl = Templ' "p.mustache" "p.out" :: Step' Ctx
        cmd  = Cmd' "echo Hello"
