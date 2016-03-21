module Cook.Catalog.User (
    addUser
  , delUser
  ) where

import Data.List

import Cook.Recipe

addUser :: Bool -> [String] -> String -> Recipe ()
addUser createHome extraGroups user =
    let homeFlag   = if createHome then ["-m"] else []
        groupsFlag = if null extraGroups then [] else ["-G", intercalate "," extraGroups]
    in withRecipeName "User.Add" $ runProc "useradd" $ homeFlag ++ groupsFlag ++ [user]

delUser :: String -> Recipe ()
delUser user = withRecipeName "User.Del" $ runProc "userdel" ["-r", user]
