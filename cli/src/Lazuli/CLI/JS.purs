module Lazuli.CLI.JS where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..))
import ArgParse.Basic as ArgParser
import Data.Array as Array
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Lazuli.CLI.Effect.FS (FS)
import Lazuli.CLI.Effect.FS as FS
import Lazuli.CLI.Effect.Log (LoggerConfig, LOG)
import Lazuli.CLI.Effect.Log as Log
import Lazuli.CLI.Node as Node
import Lazuli.CLI.Program (program, parse)
import Node.Process as Process
import Run (AFF, EFFECT, Run, runBaseAff')
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

type ErrorType = String

runNode
  :: forall a
   . LoggerConfig
  -> Run (LOG + FS + EXCEPT ErrorType + AFF + EFFECT + ()) a
  -> Aff (Either ErrorType a)
runNode loggerConfig m = m
  # Log.interpret (Node.jsConsoleHandler loggerConfig)
  # FS.interpret Node.nodeFsHandler
  # Except.runExcept
  # runBaseAff'

main :: Effect Unit
main = do
  args <- Array.drop 2 <$> Process.argv
  case parse args of
    -- Asking for help is not a failure, and a shell that checks the exit
    -- status should not be told it was one.
    Left err@(ArgError _ ShowHelp) -> asked err
    Left err@(ArgError _ (ShowInfo _)) -> asked err
    Left err -> do
      Console.error (ArgParser.printArgError err)
      Process.exit' 1
    Right opts -> launchAff_ (run opts)
  where
  asked err = Console.log (ArgParser.printArgError err)

  run opts =
    let
      loggerConfig = Log.defaultLoggerConfig
        { minLevel = opts.logLevel
        , color = not opts.monochrome
        }
    in
      runNode loggerConfig (program opts) >>= case _ of
        Right _ -> pure unit
        Left err -> liftEffect do
          Console.error err
          Process.exit' 1
