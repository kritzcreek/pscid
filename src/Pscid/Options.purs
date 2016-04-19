module Pscid.Options where

import Prelude
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (catchException)
import Data.Either (Either(Left))
import Data.Function.Eff (EffFn2, runEffFn2)
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Global (readInt)
import Node.FS (FS)
import Node.Yargs.Applicative (flag, yarg, runY)
import Node.Yargs.Setup (example, usage)

type PscidOptions =
  { port           ∷ Int
  , buildCommand   ∷ String
  , testCommand    ∷ String
  , buildAfterSave ∷ Boolean
  , testAfterSave  ∷ Boolean
  }

defaultOptions ∷ PscidOptions
defaultOptions =
  { port: 4243
  , buildCommand: "pulp build"
  , testCommand: "pulp test"
  , buildAfterSave: false
  , testAfterSave: false
  }

mkDefaultOptions ∷ ∀ e. Eff (fs ∷ FS | e) PscidOptions
mkDefaultOptions = do
  bc ← getBuildScript
  pure (defaultOptions {buildCommand = fromMaybe "pulp build" bc})

optionParser :: ∀ e. Eff (console ∷ Console.CONSOLE, fs ∷ FS | e) PscidOptions
optionParser =
  let
    setup = usage "$0 -p 4245" <> example "$0 -p 4245" "Watching ... on port 4245"
  in
   catchException (const do
                      Console.error "Failed parsing the arguments."
                      Console.error "Falling back to default options"
                      mkDefaultOptions) $
     runY setup $ (\port buildAfterSave testAfterSave → do
                    mkDefaultOptions <#> _ { port = floor (readInt 10 port)
                                           , buildAfterSave = buildAfterSave
                                           , testAfterSave = testAfterSave
                                           })
       <$> yarg "p" ["port"] (Just "The Port") (Left "4243") false
       <*> flag "--build" ["build"] (Just "Build project after save")
       <*> flag "--test" ["test"] (Just "Test project after save")

foreign import getBuildScriptImpl
  ∷ ∀ e. EffFn2 (fs ∷ FS | e) (Maybe String) (String → Maybe String) (Maybe String)

getBuildScript ∷ ∀ e. Eff (fs ∷ FS | e) (Maybe String)
getBuildScript = runEffFn2 getBuildScriptImpl Nothing Just
