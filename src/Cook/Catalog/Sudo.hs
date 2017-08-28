module Cook.Catalog.Sudo (
    enableSudoWheel
  ) where

import Cook.Recipe

enableSudoWheel :: Recipe f ()
enableSudoWheel = withRecipeName "Sudo.EnableSudoWheel" $
    createFsTree "/etc/sudoers.d" tree
  where tree     = File "wheel-nopasswd" nopasswd (Just 0o440, Just ("root", "root"))
        nopasswd = Content "%wheel ALL=(ALL) NOPASSWD: ALL\n\
                           \Defaults env_keep += \"SSH_AUTH_SOCK\"\n"
