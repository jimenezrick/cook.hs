module Cook.Catalog.Sudo (
    enableSudoWheel
  ) where

import Cook.Recipe

enableSudoWheel :: Recipe ()
enableSudoWheel = createFsTree "/etc/sudoers.d" tree
    where tree     = File "wheel-nopasswd" nopasswd (Just 0o440, Just ("root", "root"))
          nopasswd = Content "%wheel ALL=(ALL) NOPASSWD: ALL"
