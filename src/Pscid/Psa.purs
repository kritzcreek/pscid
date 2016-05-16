module Pscid.Psa
       ( module Psa
       , parseErrors
       , psaPrinter
       ) where

import Prelude
import Control.Monad.Eff.Console as Console
import Data.Array as Array
import Data.Set as Set
import Data.String as Str
import Node.Encoding as Encoding
import Node.FS.Sync as File
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Data.Argonaut (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (head, null)
import Data.Either (Either)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.String (joinWith)
import Data.Traversable (traverse)
import Node.FS (FS)
import Psa (PsaError, PsaResult, parsePsaError, Output, PsaOptions, output)
import Psa.Printer (renderAnsi, renderRow)
import Psa.Printer.Default (renderError, renderWarning)
import Psa.Util (iter_)
import Pscid.Util (shush, (∘))

defaultOptions ∷ PsaOptions
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

print ∷ forall eff. String → PsaOptions → Output → Eff (console ∷ Console.CONSOLE | eff) Unit
print successMessage options {warnings, errors} = do
  iter_ warnings \i warning → do
    Console.error (toString (renderWarning 1 1 warning))
    Console.error ""

  iter_ errors \i error → do
    Console.error (toString (renderError 1 1 error))
    Console.error ""

  when (null warnings && null errors)
    (Console.error successMessage)
  where
    toString = renderRow (joinWith "" ∘ map (renderAnsi options.ansi))

parseErrors ∷ Json → Either String (Array PsaError)
parseErrors j = traverse parsePsaError =<< decodeJson j

parseFirstError ∷ Json → Maybe PsaError
parseFirstError j = head =<< (shush (parseErrors j))

emptyResult ∷ PsaResult
emptyResult = {warnings: [], errors: []}

wrapError ∷ Boolean → PsaError → PsaResult
wrapError b e = if b
                then { warnings: [ ], errors: [e] }
                else { warnings: [e], errors: [ ] }

psaPrinter
  ∷ ∀ eff
  . String
  → Boolean
  → Array PsaError
  → Eff ( console ∷ Console.CONSOLE, fs ∷ FS | eff) Unit
psaPrinter successMessage isError errs =
  catchException (const (Console.error "An error inside psaPrinter")) do
    out' ← output loadLines defaultOptions result
    print successMessage defaultOptions out'
    where
      result = fromMaybe emptyResult (wrapError isError <$> head errs)

loadLines
  ∷ ∀ a e
  . String
  -> { startLine :: Int , endLine :: Int | a}
  -> Eff ( fs :: FS, err :: EXCEPTION | e) (Maybe (Array String))
loadLines filename pos = do
  contents ← Str.split "\n" <$> File.readTextFile Encoding.UTF8 filename
  let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
  pure (Just source)
