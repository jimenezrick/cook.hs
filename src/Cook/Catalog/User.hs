module Cook.Catalog.User (
    addUser
  , delUser
  , addGroup
  ) where

import Data.List (intercalate)

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

addGroup :: Bool -> String -> Recipe ()
addGroup isSystem group =
    let systemFlag   = if isSystem then ["--system"] else []
    in withRecipeName "Group.Add" $ do
        withoutErrorWhen groupAlreadyExists $ runProc "groupadd" $ systemFlag ++ [group]
  where groupAlreadyExists 9 = Just ()
        groupAlreadyExists _ = Nothing
