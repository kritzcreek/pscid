module Pscid.Process where

import Prelude
import Ansi.Codes (Color(Green, Red))
import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.ST (readSTRef, modifySTRef, newSTRef, runST)
import Data.Array (uncons)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.String (split)
import Node.ChildProcess (Exit(BySignal, Normally), onExit, stderr, stdout, defaultSpawnOptions, spawn, CHILD_PROCESS)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (onDataString)
import Partial.Unsafe (unsafePartial)
import Pscid.Console (logColored)
import Pscid.Error (catchLog)

execCommand
  ∷ ∀ e
  . String
  → String
  → Eff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE | e) Unit
execCommand name command =
   catchLog (name <> " threw an exception") $
    runST do
      let cmd = unsafePartial fromJust (uncons (split " " command))
      output ← newSTRef ""
      log ("Running: \"" <> command <> "\"")
      cp ← spawn cmd.head cmd.tail defaultSpawnOptions

      let stout = stdout cp
          sterr = stderr cp

      onDataString stout UTF8 \s →
        modifySTRef output (_ <> s) $> unit

      onDataString sterr UTF8 \s →
        modifySTRef output (_ <> s) $> unit

      onExit cp \e → case e of
        Normally 0 → logColored Green (name <> " successful!")
        Normally code → do
          log =<< readSTRef output
          logColored Red (name <> " errored with code: " <> show code)
        BySignal _       → pure unit

