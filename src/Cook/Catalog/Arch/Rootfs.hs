module Cook.Catalog.Arch.Rootfs (
    buildRootfs
  ) where

import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import qualified Data.Text as T

import Cook.Recipe
import Cook.Recipe.Util

buildRootfs :: FilePath -> [String] -> Recipe f FilePath
buildRootfs path extraPkgs = withRecipeName "Arch.Rootfs.BuildRootfs" $ do
    liftIO $ createDirectory path
    runProc "pacstrap" $ ["-i", "-c", "-d", path, "base"] ++ extraPkgs ++ ["--ignore", "linux"]
    enablePts0 $ path </> "etc/securetty"
    return path

enablePts0 :: FilePath -> Recipe f ()
enablePts0 securetty = withRecipeName "EnablePts0" $
    mapFileContent securetty $ T.unlines . insertPts0 . T.lines
  where insertPts0 ls = init ls ++ ["pts/0", "", last ls]
