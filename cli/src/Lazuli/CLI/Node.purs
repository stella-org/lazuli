module Lazuli.CLI.Node where

import Prelude

import Dodo as Dodo
import Dodo.Ansi (foreground)
import Dodo.Ansi as Ansi
import Effect.Aff (attempt)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Data.Array as Array
import Data.String (Pattern(..), Replacement(..))
import Data.String as String
import Data.Bifunctor (bimap, lmap)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.FS.Perms as Perms
import Node.Glob.Basic (expandGlobs)
import Node.Path as Path
import Run (AFF, EFFECT, Run, liftEffect)
import Run as Run
import Lazuli.CLI.Effect.FS (FileSystem(..))
import Lazuli.CLI.Effect.Log (Log(..), LogLevel(..), LoggerConfig)
import Type.Row (type (+))

jsConsoleHandler :: forall r. LoggerConfig -> Log ~> Run (EFFECT + r)
jsConsoleHandler conf = case _ of
  Log level msg next -> do
    when (level >= conf.minLevel) do
      let
        -- a colour-coded level tag, then a space, then the (uncoloured) message.
        doc =
          if conf.minLevel > Debug then msg
          else case level of
            Debug -> foreground Ansi.Blue (Dodo.text "[DEBUG]") <> Dodo.space <> msg
            Info -> foreground Ansi.Green (Dodo.text "[INFO]") <> Dodo.space <> msg
            Warn -> foreground Ansi.Yellow (Dodo.text "[WARN]") <> Dodo.space <> msg
            Error -> foreground Ansi.Red (Dodo.text "[ERROR]") <> Dodo.space <> msg
        printed =
          if conf.color then Dodo.print Ansi.ansiGraphics Dodo.twoSpaces doc
          else Dodo.print Dodo.plainText Dodo.twoSpaces doc
        -- Error always to stderr; Warn to stderr only under `--strict`; the rest to stdout.
        emit = case level of
          Error -> Console.error
          Warn | conf.strict -> Console.error
          _ -> Console.log
      liftEffect $ emit printed
    pure next

nodeFsHandler :: forall r. FileSystem ~> Run (AFF + EFFECT + r)
nodeFsHandler = case _ of
  ReadText path reply -> do
    read <- Run.liftAff (attempt (FS.readTextFile UTF8 path))
    pure (reply (lmap message read))
  WriteText path text reply -> do
    written <- Run.liftAff (attempt (FS.writeTextFile UTF8 path text))
    pure (reply (lmap message written))
  MakeDirectory path reply -> do
    made <- Run.liftAff (attempt (FS.mkdir' path { recursive: true, mode: Perms.mkPerms Perms.all Perms.all Perms.all }))
    pure (reply (lmap message made))
  Remove path reply -> do
    removed <- Run.liftAff (attempt (FS.rm' path { force: true, maxRetries: 0, recursive: false, retryDelay: 0 }))
    pure (reply (lmap message removed))
  IsAbsolute path reply -> pure (reply (Path.isAbsolute path))
  Glob root patterns reply -> do
    found <- Run.liftAff (attempt (expandGlobs root patterns))
    pure (reply (bimap message (Array.sort <<< map (slashed <<< Path.relative root) <<< Array.fromFoldable) found))
  where
  slashed path = if Path.sep == "/" then path else String.replaceAll (Pattern Path.sep) (Replacement "/") path
