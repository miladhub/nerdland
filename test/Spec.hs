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

start =
  MockState {
    events = MockEvents {
      pcCmds    = [up, quit],
      npcCmds   = [down],
      natEvents = []
    }
  , msgs = []
  }
myLoop   = game initialWorld [] :: MockChannel World
final    = snd $ runState myLoop start

main :: IO ()
main = hspec $ do
  describe "Game" $ do
    it "does not log player actions" $ do
      containsString "milad" (msgs final) `shouldBe` False

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
