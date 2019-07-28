module SimplyTyped.Main where

import qualified Data.Map.Strict as Map
import SimplyTyped.Cli
import SimplyTyped.NiceRepl
import SimplyTyped.Prelude

type ReplState = ()

initReplState :: ReplState
initReplState = ()

optCommands :: OptionCommands ReplState
optCommands = Map.empty

execCommand :: Command ReplState
execCommand = const (pure ReplContinue)

replDef :: ReplDef ReplState
replDef =
  ReplDef
    {rdGreeting = "hello", rdInitState = initReplState, rdAdditionalCommands = optCommands, rdExecCommand = execCommand}

exe :: IO ()
exe = runRepl replDef
