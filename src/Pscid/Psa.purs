module Pscid.Psa where

import Prelude
import Control.Monad.Eff.Console as Console
import Data.Array as Array
import Data.Set as Set
import Data.String as Str
import Node.Encoding as Encoding
import Node.FS.Sync as File
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console.Unsafe (logAny)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head, null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Node.FS (FS)
import Psa (Output, PsaResult, parsePsaError, PsaOptions, output)
import Psa.Printer (renderAnsi, renderRow)
import Psa.Printer.Default (renderError, renderWarning)
import Psa.Util (iter_)

defaultOptions :: PsaOptions
defaultOptions =
  { ansi: true
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , verboseStats: false
  , libDirs: []
  , strict: false
  , cwd: ""
  }

type MainEff eff =
  ( console :: Console.CONSOLE
  , err :: EXCEPTION
  , fs :: FS
  | eff
  )

print :: forall eff. PsaOptions -> Output -> Eff (console :: Console.CONSOLE | eff) Unit
print options {warnings, errors} = do
  iter_ warnings \i warning -> do
    Console.error $ toString (renderWarning 1 1 warning)
    Console.error ""

  when (null errors) (Console.error "NO ERRORS?!?!?!")

  iter_ errors \i error -> do
    Console.error $ toString (renderError 1 1 error)
    Console.error ""

  -- Console.error $ toString (renderStats' output.stats)

  where
  toString = renderRow (joinWith "" <<< map (renderAnsi options.ansi))

parsePscidResult :: Boolean -> Array (StrMap Json) -> Either String PsaResult
parsePscidResult isError obj =
  (if isError
  then { warnings: []
       , errors: _
       }
  else { warnings: _
       , errors: []
       }) <$> maybeToArray <<< head <$> traverse parsePsaError obj
  where
    maybeToArray (Just a) = [a]
    maybeToArray Nothing = []

psaPrinter :: forall eff. Boolean -> String -> Json -> Eff (MainEff eff) Unit
psaPrinter isError file err = do
  case decodeJson err >>= parsePscidResult isError of
    Left _ -> logAny err
    Right out -> do
      let filenames = Set.singleton file
      out' <- output loadLines defaultOptions out
      print defaultOptions out'

      where
      loadLines filename pos = do
        contents <- Str.split "\n" <$> File.readTextFile Encoding.UTF8 filename
        let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
        pure $ Just source
