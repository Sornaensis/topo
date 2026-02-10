{-# LANGUAGE DataKinds #-}

module Spec.LogActor (spec) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import Test.Hspec
import Actor.Log

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "LogActor" $ do
  it "appends log entries" $ withSystem $ \system -> do
    handle <- getSingleton system logActorDef
    appendLog handle (LogEntry LogInfo (Text.pack "hello"))
    snapshot <- getLogSnapshot handle
    length (lsEntries snapshot) `shouldBe` 1

  it "tracks collapsed state" $ withSystem $ \system -> do
    handle <- getSingleton system logActorDef
    setLogCollapsed handle True
    snapshot <- getLogSnapshot handle
    lsCollapsed snapshot `shouldBe` True

  it "scrolls log entries" $ withSystem $ \system -> do
    handle <- getSingleton system logActorDef
    scrollLog handle 5
    snapshot <- getLogSnapshot handle
    lsScroll snapshot `shouldBe` 5

  it "filters log entries by minimum level" $ withSystem $ \system -> do
    handle <- getSingleton system logActorDef
    appendLog handle (LogEntry LogDebug (Text.pack "debug"))
    appendLog handle (LogEntry LogInfo (Text.pack "info"))
    appendLog handle (LogEntry LogWarn (Text.pack "warn"))
    appendLog handle (LogEntry LogError (Text.pack "error"))
    setLogMinLevel handle LogWarn
    snapshot <- getLogSnapshot handle
    fmap leLevel (lsEntries snapshot) `shouldBe` [LogWarn, LogError]
