module Pscid.Psa
  ( module Psa
  , parseErrors
  , psaPrinter
  , filterWarnings
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Array as Array
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console as Console
import Effect.Exception (catchException)
import Node.Encoding as Encoding
import Node.FS.Sync as File
import Psa (Output, PsaError, PsaOptions, PsaResult, StatVerbosity(..), output, parsePsaError)
import Psa.Printer (renderAnsi, renderRow)
import Psa.Printer.Default (renderError, renderWarning)
import Psa.Util (iter_)
import Pscid.Util ((∘))

defaultOptions ∷ PsaOptions
defaultOptions =
  { ansi: true
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , libDirs: []
  , strict: false
  , cwd: ""
  , statVerbosity: NoStats
  }

print ∷ String → PsaOptions → Output → Effect Unit
print successMessage options {warnings, errors} = do
  iter_ warnings \i warning → do
    Console.error (toString (renderWarning 1 1 warning))
    Console.error ""

  iter_ errors \i error → do
    Console.error (toString (renderError 1 1 error))
    Console.error ""

  when (Array.null warnings && Array.null errors)
    (Console.error successMessage)
  where
    toString = renderRow (String.joinWith "" ∘ map (renderAnsi options.ansi))

parseErrors ∷ Json → Either String (Array PsaError)
parseErrors j = traverse parsePsaError =<< decodeJson j

parseFirstError ∷ Json → Maybe PsaError
parseFirstError j = Array.head =<< hush (parseErrors j)

emptyResult ∷ PsaResult
emptyResult = {warnings: [], errors: []}

wrapError ∷ Boolean → PsaError → PsaResult
wrapError b e = if b
                then { warnings: [ ], errors: [e] }
                else { warnings: [e], errors: [ ] }

psaPrinter ∷ String → Boolean → Array PsaError → Effect Unit
psaPrinter successMessage isError errs =
  catchException (const (Console.error "An error inside psaPrinter")) do
    out' ← output loadLines defaultOptions result
    print successMessage defaultOptions out'
    where
      result = fromMaybe emptyResult (wrapError isError <$> Array.head errs)

loadLines
  ∷ ∀ a
  . String
  → { startLine ∷ Int , endLine ∷ Int | a}
  → Effect (Maybe (Array String))
loadLines filename pos = do
  contents ← String.split (String.Pattern "\n") <$> File.readTextFile Encoding.UTF8 filename
  let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
  pure (Just source)

filterWarnings ∷ Array String → Array PsaError → Array PsaError
filterWarnings ignored errors =
  Array.filter (\e → e.errorCode `Array.notElem` ignored) errors
