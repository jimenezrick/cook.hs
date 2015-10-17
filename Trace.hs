module Trace where

import Control.Monad.Catch
import Control.Monad.State
{-import Control.Monad.Except-}
import System.Exit

import Deploy

{-type Error = ([String], Int)-}

{-type Receipt = ExceptT Error (StateT [String] StepM)-}
type Receipt = MonadCatch (StateT [String] StepM)

run :: Step a -> Receipt a
run step = do
    a <- liftStepM $ Deploy.run step
    state $ \s -> (a, s ++ [show step])
    return a
  where liftStepM = lift . lift
