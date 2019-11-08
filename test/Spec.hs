{-# LANGUAGE FlexibleInstances #-}

import App
import Data.Map (fromList)
import Control.Monad.State (State(..), put, get, runState)
import Test.Hspec
import Data.List (isInfixOf)

pcStats = Stats {
    x    = 0
  , y    = 10
  , life = 10
  }
npcStats = Stats {
    x    = 4
  , y    = 10
  , life = 10
  }


initialWorld = World {
    player   = "milad"
  , stats    = fromList [ ("milad", pcStats), ("ogre", npcStats) ]
  , running  = True
  }

up   = Just (Move U)
down = Just (Move D)
quit = Just Quit
help = Just Help

myNext   = next initialWorld :: MockChannel ([Event], World)
justQuit = MockState {
    events = MockEvents {
      pcCmds    = [quit],
      npcCmds   = [Nothing],
      natEvents = [Nothing]
    }
  , msgs = []
  }
afterQuit = runState myNext justQuit

start =
  MockState {
    events = MockEvents {
      pcCmds    = [up, quit],
      npcCmds   = [down, Nothing],
      natEvents = [Nothing, Nothing]
    }
  , msgs = []
  }
myLoop   = loop initialWorld :: MockChannel ()
final    = runState myLoop start

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "stop after quit command" $ do
      (running $ snd $ fst afterQuit) `shouldBe` False
    it "does not log player actions" $ do
      containsString "milad" (msgs $ snd final) `shouldBe` False
    it "logs npc actions" $ do
      containsString "ogre" (msgs $ snd final) `shouldBe` True

--
-- Test bindings
--

containsString :: String -> [String] -> Bool
containsString s ss = elem True $ isInfixOf s <$> ss

data MockState = MockState {
    events :: MockEvents
  , msgs   :: [String]
  }
  deriving (Eq, Show)

data MockEvents = MockEvents {
    pcCmds    :: [Maybe Cmd]
  , npcCmds   :: [Maybe Cmd]
  , natEvents :: [Maybe Event]
  }
  deriving (Eq, Show)

type MockChannel = State MockState

instance Channel MockChannel where
  pcCmd _ = do
    state <- get
    let es = events state
    put state { events = es { pcCmds = tail (pcCmds es) } }
    return $ head (pcCmds es)
  npcCmd _ = do
    state <- get
    let es = events state
    put state { events = es { npcCmds = tail (npcCmds es) } }
    return $ head (npcCmds es)
  nature = do
    state <- get
    let es = events state
    put state { events = es { natEvents = tail (natEvents es) } }
    return $ head (natEvents es)
  display d = do
    state <- get
    put state { msgs = (msgs state) ++ [d] }
    return ()

