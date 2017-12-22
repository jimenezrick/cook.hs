module Cook.Catalog.User (
    addUser
  , delUser
  , addGroup
  ) where

import Data.List (intercalate)

import Cook.Recipe

addUser :: Bool -> [String] -> String -> Recipe f ()
addUser createHome extraGroups user =
    let homeFlag = ["--create-home" | createHome]
        groupsFlag = if null extraGroups then [] else ["--groups", intercalate "," extraGroups]
    in withRecipeName "User.Add" $
        withoutErrorWhen userAlreadyExists $ runProc "useradd" $ homeFlag ++ groupsFlag ++ [user]
  where userAlreadyExists 9 = Just ()
        userAlreadyExists _ = Nothing

delUser :: String -> Recipe f ()
delUser user = withRecipeName "User.Delete" $
    withoutErrorWhen userDontExists $ runProc "userdel" ["--remove", user]
  where userDontExists 6 = Just ()
        userDontExists _ = Nothing

addGroup :: Bool -> String -> Recipe f ()
addGroup isSystem group =
    let systemFlag = ["--system" | isSystem]
    in withRecipeName "Group.Add" $
        withoutErrorWhen groupAlreadyExists $ runProc "groupadd" $ systemFlag ++ [group]
  where groupAlreadyExists 9 = Just ()
        groupAlreadyExists _ = Nothing
