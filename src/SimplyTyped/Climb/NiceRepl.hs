{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Climb.NiceRepl where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad (forever, unless)
import Control.Monad.Catch (throwM)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, put)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude
import SimplyTyped.Climb.Cli
import SimplyTyped.Climb.Exceptions

data ReplExc
  = ExpectedNoInputError
  | MissingCommandError Text
  | CommandError
  deriving (Generic, Eq, Show, Typeable)

instance Exception ReplExc

throwCommandError :: Cli s a
throwCommandError = throwM CommandError

printCatchCommand :: Command s -> Command s
printCatchCommand command input = printCatch "Repl" ReplContinue (tyMatch (Proxy :: Proxy ReplExc)) (command input)

assertEmpty :: Text -> Cli s ()
assertEmpty input = unless (T.null input) (throwM ExpectedNoInputError)

bareCommand :: Cli s ReplDirective -> Command s
bareCommand cmd input = assertEmpty input >> cmd

type OptionCommands s = Map Text (Text, Command s)

quitCommand :: Command s
quitCommand = bareCommand (pure ReplQuit)

helpCommand :: OptionCommands s -> Command s
helpCommand options =
  bareCommand $ do
    outputStrLn "Available commands:"
    for_ (M.toList options) $ \(name, (desc, _)) -> outputPartsLn [":", name, "\t", desc]
    pure ReplContinue

dumpCommand :: Show s => Command s
dumpCommand =
  bareCommand $ do
    outputStrLn "Repl state:"
    s <- get
    outputPretty s
    pure ReplContinue

hangCommand :: Command s
hangCommand =
  bareCommand $ do
    _ <- liftIO (forever (threadDelay maxBound))
    pure ReplContinue

clearCommand :: s -> Command s
clearCommand initState =
  bareCommand $ do
    put initState
    pure ReplContinue

defaultOptions :: Show s => s -> OptionCommands s -> OptionCommands s
defaultOptions initState allCommands =
  M.fromList
    [ ("quit", ("quit", quitCommand))
    , ("hang", ("hang the interpreter (for testing)", hangCommand))
    , ("clear", ("clear decl/defn history", clearCommand initState))
    , ("help", ("describe all commands", helpCommand allCommands))
    , ("dump", ("dump debug state", dumpCommand))
    ]

outerCommand :: OptionCommands s -> Command s -> Command s
outerCommand optionCommands execCommand input =
  case T.uncons input of
    Just (':', rest) -> do
      let (name, subInput) = T.breakOn " " rest
      case M.lookup name optionCommands of
        Nothing -> throwM (MissingCommandError name)
        Just (_, command) -> command (T.drop 1 subInput)
    _ -> execCommand input

data ReplDef s =
  ReplDef
    { rdGreeting :: Text
    , rdInitState :: s
    , rdAdditionalCommands :: OptionCommands s
    , rdExecCommand :: Command s
    }
  deriving (Generic)

niceRepl :: Show s => ReplDef s -> Cli s ()
niceRepl (ReplDef greeting initState addl exec) = do
  let options = fix (\c -> defaultOptions initState c <> addl)
      cmd = outerCommand options exec
  outputStrLn greeting
  outputStrLn "Enter `:quit` to exit or `:help` to see all commands."
  repl "> " (printCatchCommand cmd)

runRepl :: Show s => ReplDef s -> IO ()
runRepl rd = execCli (niceRepl rd) (rdInitState rd) $> ()
