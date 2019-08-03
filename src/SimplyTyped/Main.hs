{-# LANGUAGE OverloadedLists #-}

module SimplyTyped.Main where

import qualified Data.Map.Strict as Map
import SimplyTyped.Back
import SimplyTyped.Climb
import SimplyTyped.Convert
import SimplyTyped.Front
import SimplyTyped.Prelude
import SimplyTyped.Tree

type ReplState = ()

initReplState :: ReplState
initReplState = ()

safeTyProxies :: Seq TyProxy
safeTyProxies = [TyProxy (Proxy :: Proxy NoParseError), TyProxy (Proxy :: Proxy (AmbiguityError FrontFix))]

safely :: Cli ReplState ReplDirective -> Cli ReplState ReplDirective
safely = printCatch "Main" ReplContinue (runTyProxies safeTyProxies)

printGrammarFor :: Treeable a => Proxy a -> Command s
printGrammarFor p =
  bareCommand $ do
    outputStrLn "Ref:"
    let ref = refTree p
    outputPretty ref
    outputStrLn "Grammar:"
    let grammar = defineTree p
    outputPretty grammar
    outputStrLn "Deps:"
    let deps = runCrawlDeps p
    outputPretty deps
    pure ReplContinue

optCommands :: OptionCommands ReplState
optCommands =
  Map.fromList
    [ ("frontgrammar", ("display frontend expression grammar", printGrammarFor (Proxy :: Proxy FrontFix)))
    , ("backgrammar", ("display backend expression grammar", printGrammarFor (Proxy :: Proxy ExpScope)))
    ]

execCommand :: Command ReplState
execCommand t =
  safely $ do
    a <- easyReadTreeable (Proxy :: Proxy FrontFix) t
    outputStrLn "Parsed:"
    outputPretty a
    outputStrLn "Converted:"
    let c = convert a
    outputPretty c
    pure ReplContinue

replDef :: ReplDef ReplState
replDef =
  ReplDef
    {rdGreeting = "hello", rdInitState = initReplState, rdAdditionalCommands = optCommands, rdExecCommand = execCommand}

exe :: IO ()
exe = runRepl replDef
