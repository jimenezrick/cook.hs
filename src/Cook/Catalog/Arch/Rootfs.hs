module Cook.Catalog.Arch.Rootfs (
    buildRootfs
  ) where

import System.Directory
import System.FilePath

import qualified Data.Text as T

import Cook.Recipe
import Cook.Recipe.Util

buildRootfs :: FilePath -> [String] -> Recipe f FilePath
buildRootfs path extraPkgs = withRecipeName "Arch.Rootfs.BuildRootfs" $ do
    recipeIO $ createDirectory path
    runProc "pacstrap" $ ["-i", "-c", "-d", path, "base"] ++ extraPkgs ++ ["--ignore", "linux"]
    enablePts0 $ path </> "etc/securetty"
    return path

enablePts0 :: FilePath -> Recipe f ()
enablePts0 securetty = withRecipeName "EnablePts0" $
    mapFileLines securetty insertPts0
  where insertPts0 ls = init ls ++ ["pts/0", "", last ls]
