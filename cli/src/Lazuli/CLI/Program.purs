module Lazuli.CLI.Program where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParser
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Lazuli.CLI.Command.Build as Build
import Lazuli.CLI.Command.Options (loglevel)
import Lazuli.CLI.Command.Repl as Repl
import Lazuli.CLI.Command.Run as Run
import Lazuli.CLI.Effect.FS (FS)
import Lazuli.CLI.Effect.Log (LogLevel(..), LOG)
import Run (EFFECT, Run, AFF)
import Run.Except (EXCEPT)
import Type.Row (type (+))

data Command
  = Build Build.Options
  | Run Run.Options
  | Repl Repl.Options

derive instance Generic Command _
instance Show Command where
  show = genericShow

type Options =
  { logLevel :: LogLevel
  , monochrome :: Boolean
  , command :: Command
  }

options :: ArgParser Options
options =
  ArgParser.fromRecord
    { logLevel:
        ArgParser.argument [ "--log-level" ]
          "Suppress log messages of level lower than"
          # loglevel
          # ArgParser.default Info
    , monochrome:
        ArgParser.flag [ "--monochrome" ]
          "Disable coloring log messages"
          # ArgParser.boolean
    , command:
        ArgParser.choose "command"
          [ ArgParser.command [ "build" ]
              "Build the project"
              ((Build <$> buildOptions) <* ArgParser.flagHelp)
          , ArgParser.command [ "run" ]
              "Run the project on STEAM"
              ((Run <$> runOptions) <* ArgParser.flagHelp)
          , ArgParser.command [ "repl" ]
              "Start a REPL"
              ((Repl <$> ArgParser.fromRecord {}) <* ArgParser.flagHelp)
          ]
    }
    <* ArgParser.flagHelp
  where
  buildOptions = ArgParser.fromRecord
    { stellacExec:
        ArgParser.argument [ "-x", "--stellac-cmd" ]
          "Path to an executable of stellac"
          # ArgParser.default "stellac"
    , stellacArgs:
        ArgParser.argument [ "--stellac-args" ]
          "Additional arguments to pass to stellac"
          # ArgParser.optional
    , config:
        ArgParser.argument [ "-c", "--config" ]
          "Path to a lazuli config file\n\
          \Defaults to lazuli.yaml in the current working directory"
          # ArgParser.default "lazuli.yaml"
    }

  runOptions = ArgParser.fromRecord
    {}

parse :: Array String -> Either ArgParser.ArgError Options
parse = ArgParser.parseArgs "lazuli" "The Lazuli Package Manager" options

type ErrorType = String

program :: Options -> Run (LOG + FS + EXCEPT ErrorType + EFFECT + AFF + ()) Unit
program opts = case opts.command of
  Build buildOpts -> Build.cmd buildOpts
  Run runOpts -> Run.cmd runOpts
  Repl replOpts -> Repl.cmd replOpts