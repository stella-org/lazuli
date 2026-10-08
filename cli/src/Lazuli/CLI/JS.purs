module Lazuli.CLI.JS where

import Prelude

import Effect (Effect)
import Effect.Console as Console

main :: Effect Unit
main = do
  Console.log "Lazuli CLI"
