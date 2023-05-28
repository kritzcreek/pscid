module Main where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Argonaut (Json)
import Data.Array as Array
import Data.Either (Either(..), either, isRight)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Milliseconds(..), attempt, delay, launchAff_, runAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.Process as Process
import Psa (PsaError)
import PscIde (cwd, load, sendCommandR)
import PscIde.Command (Command(..), Message(..))
import Pscid.Console (clearConsole, owl, startScreen, suggestionHint)
import Pscid.Error (catchLog, noSourceDirectoryError)
import Pscid.Keypress (Key(..), initializeKeypresses, onKeypress)
import Pscid.Options (PscidSettings, optionParser, printCLICommand)
import Pscid.Process (execCommand)
import Pscid.Psa (filterWarnings, parseErrors, psaPrinter)
import Pscid.Server (restartServer, startServer', stopServer')
import Pscid.Util (both, (∘))
import Suggest (applySuggestions)
import Type.Prelude as TE

newtype Pscid a = Pscid (ReaderT (PscidSettings Int) Effect a)
derive newtype instance functorPscid ∷ Functor Pscid
derive newtype instance applyPscid ∷ Apply Pscid
derive newtype instance applicativePscid ∷ Applicative Pscid
derive newtype instance bindPscid ∷ Bind Pscid
derive newtype instance monadPscid ∷ Monad Pscid
instance monadAskPscid ∷ TE.TypeEquals r (PscidSettings Int) ⇒ MonadAsk r Pscid where
  ask = Pscid (map TE.from ask)
instance monadEffectPscid ∷ MonadEffect Pscid where
  liftEffect = Pscid ∘ liftEffect

runPscid ∷ ∀ a. Pscid a → PscidSettings Int → Effect a
runPscid (Pscid f) e = runReaderT f e

newtype State = State { errors ∷ Array PsaError }

emptyState ∷ State
emptyState = State { errors: [] }

main ∷ Effect Unit
main = launchAff_ do
  config@{ port, outputDirectory, sourceDirectories } ← liftEffect optionParser
  when (Array.null sourceDirectories) (liftEffect noSourceDirectoryError)
  stateRef ← liftEffect (Ref.new emptyState)
  Console.log "Starting purs ide server"
  r ← attempt (startServer' port outputDirectory)
  case r of
    Right (Right port') → do
      let config' = config { port = port' }
      Message directory ← do
        delay (Milliseconds 500.0)
        _ ← load port' [] []
        res ← cwd port'
        case res of
          Right d → pure d
          Left err → liftEffect do
            Console.log err
            Process.exit 1
      liftEffect do
        runEffectFn2 gaze
          (Array.concatMap fileGlob sourceDirectories)
          (\d → runPscid (triggerRebuild stateRef d) config')
        clearConsole
        initializeKeypresses
        onKeypress (\k → runPscid (keyHandler stateRef k) config')
        Console.log ("Watching " <> directory <> " on port " <> show port')
        startScreen
    Right (Left errMsg) →
      Console.log ("Failed to start psc-ide-server with: " <> errMsg)
    Left err →
      Console.log ("Failed to start psc-ide-server with : " <> show err)

-- | Given a directory, appends the globs necessary to match all PureScript and
-- | JavaScript source files inside that directory
fileGlob ∷ String → Array String
fileGlob dir =
  let go x = dir <> "/**/*" <> x
  in go <$> [".purs", ".js"]

keyHandler ∷ Ref State → Key → Pscid Unit
keyHandler stateRef k = do
  {port, buildCommand, outputDirectory, testCommand} ← ask
  case k of
    Key {ctrl: false, name: "b", meta: false} →
      liftEffect (execCommand "Build" $ printCLICommand buildCommand)
    Key {ctrl: false, name: "t", meta: false} →
      liftEffect (execCommand "Test" $ printCLICommand testCommand)
    Key {ctrl: false, name: "r", meta: false} → liftEffect do
      clearConsole
      catchLog "Failed to restart server" $ launchAff_ do
        restartServer port outputDirectory
        _ ← load port [] []
        pure unit
      Console.log owl
    Key {ctrl: false, name: "s", meta: false} → liftEffect do
      State state ← Ref.read stateRef
      case Array.head state.errors of
        Nothing →
          Console.log "No suggestions available"
        Just e →
          catchLog "Couldn't apply suggestion." (applySuggestions [e])
    Key {ctrl: false, name: "q", meta: false} →
      liftEffect (Console.log "Bye!" <* runAff (either exit exit) (stopServer' port))
    Key {ctrl: true, name: "c", meta: false} →
      Console.log "Press q to exit"
    Key {name} →
      Console.log name
  where
    exit ∷ ∀ a. a → Effect Unit
    exit = const (Process.exit 0)

triggerRebuild ∷ Ref State → String → Pscid Unit
triggerRebuild stateRef file = do
  {port, testCommand, testAfterRebuild, censorCodes} ← ask
  let fileName = changeExtension file "purs"
  liftEffect ∘ catchLog "We couldn't talk to the server" $ launchAff_ do
    result ← sendCommandR port (RebuildCmd fileName Nothing Nothing)
    case result of
      Left _ → Console.log "We couldn't talk to the server"
      Right errs → liftEffect do
        parsedErrors ← handleRebuildResult fileName censorCodes errs
        Ref.write (State {errors: parsedErrors}) stateRef
        case Array.head parsedErrors >>= _.suggestion of
          Nothing → pure unit
          Just _ → suggestionHint
        when (testAfterRebuild && isRight errs)
          (execCommand "Test" $ printCLICommand testCommand)

changeExtension ∷ String → String → String
changeExtension s ex =
  case String.lastIndexOf (Pattern ".") s of
    Nothing →
      s
    Just ix →
      String.take ix s <> "." <> ex

handleRebuildResult
  ∷ String
  → Array String
  → Either Json Json
  → Effect (Array PsaError)
handleRebuildResult file censorCodes result = do
  clearConsole
  Console.log ("Checking " <> file)
  case both parseErrors result of
    Right warnings →
      either
        (\_ → Console.log "Failed to parse warnings" $> [])
        (\e → psaPrinter owl false e $> e)
        (filterWarnings censorCodes <$> warnings)
    Left errors →
      either
        (\_ → Console.log "Failed to parse errors" $> [])
        (\e → psaPrinter owl true e $> e)
        errors

foreign import gaze ∷ EffectFn2 (Array String) (String → Effect Unit) Unit
