{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Text.Lazy

import qualified Data.Text.Lazy.IO as T

import Cook.Recipe
import Cook.Catalog.Cjdns

main :: IO ()
main = testCjdns

testCjdns :: IO ()
testCjdns = do
    runRecipe $ do
        requireCjdns "conf/cjdns/node.yaml"
