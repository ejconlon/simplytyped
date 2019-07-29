{-# LANGUAGE OverloadedLists #-}

module SimplyTyped.Main where

import qualified Data.Map.Strict as Map
import SimplyTyped.Cli
import SimplyTyped.Exceptions
import SimplyTyped.Front
import SimplyTyped.NiceRepl
import SimplyTyped.Prelude
import SimplyTyped.Tree

type ReplState = ()

initReplState :: ReplState
initReplState = ()

safeTyProxies :: Seq TyProxy
safeTyProxies =
  [ TyProxy (Proxy :: Proxy NoParseError)
  , TyProxy (Proxy :: Proxy (AmbiguityError FrontFix))
  ]

safely :: Cli ReplState ReplDirective -> Cli ReplState ReplDirective
safely = printCatch "Main" ReplContinue (runTyProxies safeTyProxies)

optCommands :: OptionCommands ReplState
optCommands = Map.empty

execCommand :: Command ReplState
execCommand t = safely $ do
  a <- easyReadTreeable (Proxy :: Proxy FrontFix) t
  outputStrLn "Parsed:"
  outputPretty a
  pure ReplContinue

replDef :: ReplDef ReplState
replDef =
  ReplDef
    {rdGreeting = "hello", rdInitState = initReplState, rdAdditionalCommands = optCommands, rdExecCommand = execCommand}

exe :: IO ()
exe = runRepl replDef
