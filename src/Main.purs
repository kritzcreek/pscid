module Main where

import Prelude
import Control.Monad.Eff.Console as Console
import Node.Process as Process
import Ansi.Codes (Color(Blue, Green, Red))
import Ansi.Output (foreground, withGraphics)
import Control.Apply ((*>))
import Control.Bind ((=<<))
import Control.Monad (when)
import Control.Monad.Aff (attempt, runAff, launchAff, later')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (catchException, EXCEPTION)
import Control.Monad.Eff.Ref (writeRef, readRef, Ref, newRef, REF)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT, ReaderT)
import Control.Monad.ST (readSTRef, modifySTRef, newSTRef, runST)
import Control.Monad.Trans (lift)
import Data.Argonaut (Json)
import Data.Array (head, filter, uncons)
import Data.Either (isRight, isLeft, Either(Left, Right), either)
import Data.Foldable (notElem)
import Data.Function.Eff (runEffFn2, EffFn2)
import Data.Functor (($>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.String (split)
import Node.ChildProcess (CHILD_PROCESS, stderr, stdout, Exit(BySignal, Normally), onExit, defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.Stream (onDataString)
import PscIde (sendCommandR, load, cwd, NET)
import PscIde.Command (Command(RebuildCmd), Message(Message))
import Pscid.Keypress (Key(Key), onKeypress, initializeKeypresses)
import Pscid.Options (PscidOptions, optionParser)
import Pscid.Psa (PsaError, parseErrors, psaPrinter)
import Pscid.Server (restartServer, stopServer, startServer)
import Pscid.Util ((∘))
import Suggest (applySuggestions)

type Pscid e a = ReaderT PscidOptions (Eff e) a

newtype State = State { errors :: Array PsaError }

emptyState :: State
emptyState = State { errors: [] }

main ∷ ∀ e. Eff ( err ∷ EXCEPTION, cp ∷ CHILD_PROCESS
                , console ∷ CONSOLE , net ∷ NET
                , avar ∷ AVAR, fs ∷ FS, process ∷ Process.PROCESS
                , ref :: REF | e) Unit
main = launchAff do
  config@{ port, sourceDirectories } ← liftEff optionParser
  stateRef <- liftEff (newRef emptyState)
  liftEff (log "Starting psc-ide-server")
  r ← attempt (startServer "psc-ide-server" port Nothing)
  when (isLeft r) (restartServer port)
  Message directory ← later' 100 do
    load port [] []
    res ← cwd port
    case res of
      Right d → pure d
      Left err → liftEff do
        log err
        Process.exit 1
  liftEff do
    runEffFn2 gaze
      (sourceDirectories <#> \g → directory <> "/" <> g <> "/**/*.purs")
      (\d → runReaderT (triggerRebuild stateRef d) config)
    clearConsole
    initializeKeypresses
    onKeypress (\k → runReaderT (keyHandler stateRef k) config)
    log ("Watching " <> directory <> " on port " <> show port)
    log owl
    logColored Blue helpText

owl ∷ String
owl =
  """
  ___     ,_,        ___        ,_,     ___
 (o,o)   (o,o)   ,,,(o,o),,,   (o,o)   (o,o)
 {`"'}   {`"'}    ';:`-':;'    {`"'}   {`"'}
 -"-"-   -"-"-                 -"-"-   -"-"-
  """

helpText ∷ String
helpText =
  """
Press b to run a full build (tries "npm run build" then "pulp build")
Press t to test (tries "npm run test" then "pulp test")
Press r to reset
Press q to quit
  """

keyHandler
  ∷ ∀ e
  . Ref State
  → Key
  → Pscid ( console ∷ CONSOLE , cp ∷ CHILD_PROCESS
          , process ∷ Process.PROCESS , net ∷ NET
          , fs ∷ FS, avar ∷ AVAR, ref ∷ REF | e) Unit
keyHandler stateRef k = do
  {port, buildCommand, testCommand} ← ask
  case k of
    Key {ctrl: false, name: "b", meta: false, shift: false} →
      liftEff (runCommand "Build" buildCommand)
    Key {ctrl: false, name: "t", meta: false, shift: false} →
      liftEff (runCommand "Test" testCommand)
    Key {ctrl: false, name: "r", meta: false, shift: false} → liftEff do
      clearConsole
      catchLog "Failed to restart server" $ launchAff do
        restartServer port
        load port [] []
      log owl
    Key {ctrl: false, name: "s", meta: false, shift: false} → liftEff do
      State state ← readRef stateRef
      case head state.errors of
        Nothing →
          log "No suggestions available"
        Just e →
          catchLog "Couldn't apply suggestion." (runST (applySuggestions [e]))
    Key {ctrl: false, name: "q", meta: false, shift: false} →
      liftEff (log "Bye!" *> runAff exit exit (stopServer port))
    Key {ctrl, name, meta, shift} →
      liftEff (log name)
  where
    exit ∷ ∀ a eff. a → Eff (process ∷ Process.PROCESS | eff) Unit
    exit = const (Process.exit 0)

runCommand
  ∷ ∀ e
  . String
  → String
  → Eff (cp ∷ CHILD_PROCESS, console ∷ CONSOLE | e) Unit
runCommand name command =
   catchLog (name <> " threw an exception") $
    runST do
      let cmd = fromJust (uncons (split " " command))
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

triggerRebuild
  ∷ ∀ e
  . Ref State
  → String
  → Pscid ( cp ∷ CHILD_PROCESS, net ∷ NET
          , console ∷ CONSOLE, fs ∷ FS
          , ref ∷ REF| e) Unit
triggerRebuild stateRef file = do
  {port, testCommand, testAfterRebuild, censorCodes} ← ask
  lift ∘ catchLog "We couldn't talk to the server" $ launchAff do
    result ← sendCommandR port (RebuildCmd file)
    case result of
      Left _ → liftEff (log "We couldn't talk to the server")
      Right errs → do
        parsedErrors ← liftEff (handleRebuildResult file censorCodes errs)
        liftEff (writeRef stateRef (State {errors: parsedErrors}))
        case head parsedErrors >>= _.suggestion of
          Nothing → pure unit
          Just s → liftEff (logColored Blue "Press s to automatically apply the suggestion.")
        liftEff ∘ when (testAfterRebuild && isRight errs) $
          runCommand "Test" testCommand

handleRebuildResult
  ∷ ∀ e
  . String
  → Array String
  → Either Json Json
  → Eff (console ∷ CONSOLE, fs ∷ FS | e) (Array PsaError)
handleRebuildResult file censorCodes result = do
  clearConsole
  log ("Checking " <> file)
  case both parseErrors result of
    Right warnings →
      either
        (\_ → log "Failed to parse warnings" $> [])
        (\e → psaPrinter owl false e $> e)
        (filterWarnings censorCodes <$> warnings)
    Left errors →
      either
        (\_ → log "Failed to parse errors" $> [])
        (\e → psaPrinter owl true e $> e)
        errors

filterWarnings ∷ Array String → Array PsaError → Array PsaError
filterWarnings ignored errors =
  filter (\e → e.errorCode `notElem` ignored) errors

both ∷ ∀ a b. (a → b) → Either a a → Either b b
both f e = case e of
  Right x → Right (f x)
  Left x → Left (f x)

foreign import gaze
  ∷ ∀ eff
  . EffFn2 (fs ∷ FS | eff)
      (Array String)
      (String → Eff (fs ∷ FS | eff) Unit)
      Unit

foreign import clearConsole ∷ ∀ e. Eff (console ∷ CONSOLE | e) Unit

catchLog
  ∷ ∀ e
  . String
  → Eff (console ∷ CONSOLE, err ∷ EXCEPTION | e) Unit
  → Eff (console ∷ CONSOLE | e) Unit
catchLog m = catchException (const (Console.error m))

logColored ∷ ∀ e.Color → String → Eff (console ∷ CONSOLE | e) Unit
logColored c = withGraphics log (foreground c)
