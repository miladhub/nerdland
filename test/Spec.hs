{-# LANGUAGE FlexibleInstances #-}

import App
import Data.Map (fromList, toList)
import Control.Monad.State (State(..), put, get, runState)
import Test.Hspec
import Data.List (isInfixOf)

pcStats = Stats {
    x    = 10
  , y    = 10
  , life = 10
  }
npcStats = Stats {
    x    = 10
  , y    = 10
  , life = 10
  }
initialWorld = World {
    player   = "milad"
  , stats    = fromList [ ("milad", pcStats), ("ogre", npcStats) ]
  , running  = True
  }

myLoop   = game initialWorld [] :: MockChannel World
upQuitDown =
  MockState {
    events = MockEvents {
      pcCmds    = [Just (Move U), Just Quit],
      npcCmds   = [Just (Move U)],
      natEvents = []
    }
  , msgs = []
  }
(_, final) = runState myLoop upQuitDown :: (World, MockState)

killAll = Quest
  {
    name = "Kill All"
  , done = \world ->
      let players = toList $ stats world
      in length players == 1
  }
swingLoop = game initialWorld [killAll] :: MockChannel World
swing10times =
  MockState {
    events = MockEvents {
      pcCmds    = (take 10 $ repeat $ Just Swing) ++ [Just Quit],
      npcCmds   = [],
      natEvents = []
    }
  , msgs = []
  }
(_, swinged) = runState swingLoop swing10times :: (World, MockState)

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "does not log player actions" $ do
      containsString "milad" (msgs final) `shouldBe` False
    it "enables quests" $ do
      containsString "ogre is dead" (msgs swinged) `shouldBe` True
      containsString "Quest Kill All completed!" (msgs swinged) `shouldBe` True

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
