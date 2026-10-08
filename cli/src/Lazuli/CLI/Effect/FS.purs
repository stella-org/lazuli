module Lazuli.CLI.Effect.FS
  ( FS
  , FilePath
  , FileSystem(..)
  , _fs
  , interpret
  , readText
  , writeText
  , makeDirectory
  , remove
  , glob
  , isAbsolute
  ) where

import Prelude

import Data.Either (Either)
import Prim as P
import Run (Run)
import Run as Run
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type FilePath = P.String

-- | **A failure is answered rather than thrown.** What a command makes of a file
-- | it could not read is the command's — one may refuse, another may go on — so the
-- | effect reports and does not decide.
data FileSystem a
  = ReadText P.String (Either P.String P.String -> a)
  | WriteText P.String P.String (Either P.String Unit -> a)
  | MakeDirectory P.String (Either P.String Unit -> a)
  | Remove P.String (Either P.String Unit -> a)
  | Glob P.String (P.Array P.String) (Either P.String (P.Array P.String) -> a)
  | IsAbsolute P.String (P.Boolean -> a)

derive instance Functor FileSystem

type FS r = (fs :: FileSystem | r)

_fs :: Proxy "fs"
_fs = Proxy

interpret :: forall r a. (FileSystem ~> Run r) -> Run (FS + r) a -> Run r a
interpret handler = Run.interpret (Run.on _fs handler Run.send)

readText :: forall r. P.String -> Run (FS + r) (Either P.String P.String)
readText path = Run.lift _fs (ReadText path identity)

writeText :: forall r. P.String -> P.String -> Run (FS + r) (Either P.String Unit)
writeText path text = Run.lift _fs (WriteText path text identity)

makeDirectory :: forall r. P.String -> Run (FS + r) (Either P.String Unit)
makeDirectory path = Run.lift _fs (MakeDirectory path identity)

remove :: forall r. P.String -> Run (FS + r) (Either P.String Unit)
remove path = Run.lift _fs (Remove path identity)

glob :: forall r. P.String -> P.Array P.String -> Run (FS + r) (Either P.String (P.Array P.String))
glob root patterns = Run.lift _fs (Glob root patterns identity)

isAbsolute :: forall r. P.String -> Run (FS + r) P.Boolean
isAbsolute path = Run.lift _fs (IsAbsolute path identity)
