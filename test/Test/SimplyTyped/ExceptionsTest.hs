module Test.SimplyTyped.ExceptionsTest where

import Control.Exception (toException)
import SimplyTyped.Exceptions
import SimplyTyped.Prelude
import Test.SimplyTyped.Assertions
import Test.Tasty
import Test.Tasty.HUnit

data FooError = FooError deriving (Eq, Show, Typeable)
instance Exception FooError

data BarError = BazError | QuuxError deriving (Eq, Show, Typeable)
instance Exception BarError

fooProxy :: Proxy FooError
fooProxy = Proxy

fooTyProxy :: TyProxy
fooTyProxy = TyProxy fooProxy

fooTyHandler :: TyHandler Int
fooTyHandler = TyHandler fooProxy (const 42)

barProxy :: Proxy BarError
barProxy = Proxy

barTyProxy :: TyProxy
barTyProxy = TyProxy barProxy

fooError :: FooError
fooError = FooError

fooProof :: TyProof
fooProof = TyProof fooError

bazError :: BarError
bazError = BazError

bazProof :: TyProof
bazProof = TyProof bazError

tyProxies :: [TyProxy]
tyProxies = [fooTyProxy, barTyProxy]

someFooError :: SomeException
someFooError = toException fooError

someFooProof :: TyProof
someFooProof = someTyProof someFooError

test_proxies :: TestTree
test_proxies =
  testCase "proxies" $ do
    assertTrue  (tyMatch fooProxy fooProof)
    assertFalse (tyMatch barProxy fooProof)
    assertFalse (tyMatch fooProxy bazProof)
    assertTrue  (tyMatch barProxy bazProof)
    assertTrue  (runTyProxy fooTyProxy fooProof)
    assertFalse (runTyProxy barTyProxy fooProof)
    assertFalse (runTyProxy fooTyProxy bazProof)
    assertTrue  (runTyProxy barTyProxy bazProof)
    assertTrue  (runTyProxies tyProxies fooProof)
    assertTrue  (runTyProxies tyProxies bazProof)
    assertFalse (runTyProxies [] fooProof)
    assertTrue  (runTyProxies [fooTyProxy] fooProof)
    assertFalse (runTyProxies [barTyProxy] fooProof)

test_handlers :: TestTree
test_handlers =
  testCase "handlers" $ do
    runTyHandler fooTyHandler fooProof @?= Just 42
    runTyHandler fooTyHandler bazProof @?= Nothing

test_some :: TestTree
test_some =
  testCase "some exception" $ do
    assertTrue (tyMatch fooProxy someFooProof)
