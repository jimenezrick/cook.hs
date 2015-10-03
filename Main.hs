{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# LANGUAGE ScopedTypeVariables      #-}

import Data.Data
import Data.Tagged
import Data.Yaml
import GHC.Generics

import Template

data Ctx = Ctx { foo :: String
               , bar :: Int
               , bur :: Maybe Int
               } deriving (Show, Data, Typeable, Generic)

instance FromJSON Ctx

main :: IO ()
main = useTemplate (Tagged "p.mustache" :: Template Ctx) "p.out"

--main :: IO ()
--main = do
--    tmpl <- decodeFileEither "conf.yaml"
--    case tmpl of
--        Left ex -> error $ prettyPrintParseException ex
--        Right a -> do print a
--                      let ctx  = mkGenericContext (a :: Ctx)
--                          ctx' = ctx `composeCtx` ctx2
--                      hastacheStr conf txt ctx' >>= T.putStrLn
--  where txt = "Hey {{foo}} {{> p}} ... {{bur.0}}"
--        ctx2 = mkGenericContext $ Ctx "ricardo" 28 $ Just 6666666666
