module Lazuli.CLI.Command.Run where

import Prelude

import Lazuli.CLI.Effect.Log (LOG)
import Lazuli.CLI.Effect.Log as Log
import Run (Run)
import Type.Row (type (+))

type Options = {}

cmd :: forall r. Options -> Run (LOG + r) Unit
cmd opts = do
  Log.info (show opts)
  pure unit