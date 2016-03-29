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
        err <- withoutError $ runProc "useradd" $ homeFlag ++ groupsFlag ++ [user]
        case err of
            Left code | code /= userAlreadyExists -> failWith' code
            _                                     -> return ()
  where userAlreadyExists = 9

delUser :: String -> Recipe ()
delUser user = withRecipeName "User.Delete" $ do
    err <- withoutError $ runProc "userdel" ["-r", user]
    case err of
        Left code | code /= userDontExists -> failWith' code
        _                                  -> return ()
  where userDontExists = 6
