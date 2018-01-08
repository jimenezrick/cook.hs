module Cook.Catalog.User
  ( def

  , UserOpts (..)

  , addUser
  , delUser
  , addGroup
  ) where

import Control.Monad (when)
import Data.Default.Class
import Data.List (intercalate)
import System.FilePath ((</>))

import Cook.Recipe

data UserOpts = UserOpts
    { createHome    :: Bool
    , privateHome   :: Bool
    , extraGroups   :: [String]
    , isSystemGroup :: Bool
    }

instance Default UserOpts where
    def = UserOpts { createHome = True, privateHome = False, extraGroups = [], isSystemGroup = False }

addUser :: UserOpts -> String -> Recipe f ()
addUser opts user =
    let homeFlag = ["--create-home" | createHome opts]
        groupsFlag = if null (extraGroups opts) then [] else ["--groups", intercalate "," (extraGroups opts)]
    in withRecipeName "User.Add" $ do
        withoutErrorWhen userAlreadyExists $ runProc "useradd" $ homeFlag ++ groupsFlag ++ [user]
        when (privateHome opts) $ runProc "chmod" ["700", "/home" </> user]
  where userAlreadyExists 9 = Just ()
        userAlreadyExists _ = Nothing

delUser :: String -> Recipe f ()
delUser user = withRecipeName "User.Delete" $
    withoutErrorWhen userDontExists $ runProc "userdel" ["--remove", user]
  where userDontExists 6 = Just ()
        userDontExists _ = Nothing

addGroup :: UserOpts -> String -> Recipe f ()
addGroup opts group =
    let systemFlag = ["--system" | isSystemGroup opts]
    in withRecipeName "Group.Add" $
        withoutErrorWhen groupAlreadyExists $ runProc "groupadd" $ systemFlag ++ [group]
  where groupAlreadyExists 9 = Just ()
        groupAlreadyExists _ = Nothing
