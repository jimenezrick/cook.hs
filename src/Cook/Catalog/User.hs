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
    in withRecipeName "User.Add" $ do
        withoutErrorWhen userAlreadyExists $ runProc "useradd" $ homeFlag ++ groupsFlag ++ [user]
  where userAlreadyExists 9 = Just ()
        userAlreadyExists _ = Nothing

delUser :: String -> Recipe ()
delUser user = withRecipeName "User.Delete" $ do
    withoutErrorWhen userDontExists $ runProc "userdel" ["-r", user]
  where userDontExists 6 = Just ()
        userDontExists _ = Nothing
