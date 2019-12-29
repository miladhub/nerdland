{-# LANGUAGE FlexibleInstances #-}

import App
import Data.Map (fromList)
import Control.Monad.State (State(..), put, get, runState)
import Test.Hspec
import Data.List (isInfixOf)

pcStats = Stats {
    x    = 10
  , y    = 10
  , life = 10
  }
npcStats = Stats {
    x    = 0
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
      npcCmds   = [],
      natEvents = []
    }
  , msgs = []
  }
afterQuit = runState myNext justQuit

start =
  MockState {
    events = MockEvents {
      pcCmds    = [up, quit],
      npcCmds   = [down],
      natEvents = []
    }
  , msgs = []
  }
myLoop   = game initialWorld :: MockChannel World
final    = snd $ runState myLoop start

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "stop after quit command" $ do
      (running $ snd $ fst afterQuit) `shouldBe` False
    it "does not log player actions" $ do
      containsString "milad" (msgs final) `shouldBe` False
    it "logs when npcs become visible" $ do
      containsString "see ogre" (msgs final) `shouldBe` True
    it "logs when npcs are out of sight" $ do
      containsString "lost sight of ogre" (msgs final) `shouldBe` True
  describe "Events" $ do
    it "should alert when ogre is in sight" $ do
      Just isInfixOf <*> (Just "You see ogre\n") <*> (descrEvent w1 w2 (Moved "ogre" U)) `shouldBe` (Just True)
  where
    w1 = World {
        player   = "milad"
      , stats    = fromList [ ("milad", s 1 0), ("ogre", s 13 0) ]
      , running  = True
      }
    w2 = World {   
        player   = "milad"
      , stats    = fromList [ ("milad", s 1 0), ("ogre", s 10 0) ]
      , running  = True
      }
    s x' y' = Stats {
        x    = x'
      , y    = y'
      , life = 10
      }



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
  , natEvents :: [Maybe NatEvent]
  }
  deriving (Eq, Show)

type MockChannel = State MockState

instance Channel MockChannel where
  pcCmd = do
    state <- get
    let es = events state
    put state { events = es { pcCmds = tail' (pcCmds es) } }
    return $ head' (pcCmds es)
  npcCmd = do
    state <- get
    let es = events state
    put state { events = es { npcCmds = tail' (npcCmds es) } }
    return $ head' (npcCmds es)
  nature = do
    state <- get
    let es = events state
    put state { events = es { natEvents = tail' (natEvents es) } }
    return $ head' (natEvents es)
  display d = do
    state <- get
    put state { msgs = (msgs state) ++ [d] }
    return ()

head' :: [Maybe t] -> Maybe t
head' [] = Nothing
head' l = head l

tail' :: [t] -> [t]
tail' [] = []
tail' (_:t) = t
