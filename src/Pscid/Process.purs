module Pscid.Process where

import Prelude

import Ansi.Codes (Color(..))
import Data.Array as Array
import Data.Maybe (fromJust)
import Data.String as String
import Effect (Effect)
import Effect.Console as Console
import Effect.Ref as Ref
import Node.ChildProcess (Exit(..), defaultSpawnOptions, onExit, spawn, stderr, stdout)
import Node.Encoding (Encoding(..))
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Pscid.Console (logColored)
import Pscid.Error (catchLog)

execCommand ∷ String → String → Effect Unit
execCommand name command = catchLog (name <> " threw an exception") do
   let cmd = unsafePartial fromJust (Array.uncons (String.split (String.Pattern " ") command))
   output ← Ref.new ""
   Console.log ("Running: \"" <> command <> "\"")
   cp ← spawn cmd.head cmd.tail defaultSpawnOptions

   let stout = stdout cp
       sterr = stderr cp

   onDataString stout UTF8 \s →
     Ref.modify_ (_ <> s) output

   onDataString sterr UTF8 \s →
     Ref.modify_ (_ <> s) output

   onExit cp \e → case e of
     Normally 0 → logColored Green (name <> " successful!")
     Normally code → do
       Console.log =<< Ref.read output
       logColored Red (name <> " errored with code: " <> show code)
     BySignal _       → pure unit
