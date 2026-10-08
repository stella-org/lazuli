module Lazuli.CLI.Command.Build where

import Prelude

import Data.Maybe (Maybe)
import Lazuli.CLI.Effect.Log (LOG)
import Lazuli.CLI.Effect.Log as Log
import Run (Run)
import Type.Row (type (+))

type Options =
  { stellacExec :: String
  , stellacArgs :: Maybe String
  , config :: String
  }

cmd :: forall r. Options -> Run (LOG + r) Unit
cmd opts = do
  Log.info (show opts)
  pure unit