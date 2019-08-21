{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Map (Map, fromList, keys, adjustWithKey)
import System.Random (getStdRandom, randomR)
import Control.Monad (forM_)
import Data.Maybe (catMaybes, maybe)
import System.Timeout (timeout)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State (State(..), put, get, runState)

--
-- Main
--

initialStats = Stats {
    x    = 10
  , y    = 10
  , life = 10
  }

initialWorld = World {
    player   = "milad"
  , stats    = fromList $ (,)
      <$> ["milad", "ogre"]
      <*> [initialStats]
  , running  = True
  }

main :: IO ()
main = do
  putStrLn "Starting game (press '?' for help)"
  loop initialWorld
  putStrLn "Bye!"

--
-- Game logic
--

type Player = String

data Stats = Stats {
    x     :: Int
  , y     :: Int
  , life  :: Int
  }
  deriving (Show, Eq)

data World = World {
    player  :: Player
  , stats   :: Map Player Stats
  , running :: Bool
  }
  deriving (Show, Eq)

data Action =
    Move Dir
  | Swing
  deriving (Eq, Show)

data Event =
    PlayerAction Player Action
  | Rain
  | Earthquake
  | WorldStop
  deriving (Eq, Show)

data Cmd =
    Cmd Action
  | Help
  | Quit
  | Other String
  deriving (Eq, Show)

data Dir = U | D | L | R
  deriving (Eq, Show)

class Monad m => Channel m where
  pcCmd   :: Player -> m (Maybe Cmd) 
  npc     :: Player -> m (Maybe Event)
  nature  :: m (Maybe Event)
  display :: String -> m ()

loop :: Channel m => World -> m ()
loop world = do
  (events, newWorld) <- next world
  if running newWorld
    then do
      forM_ events showDesc
      loop newWorld
    else
      return ()
  where
    showDesc = maybe (return ()) display . describe

next :: Channel m => World -> m ([Event], World)
next world = do
  display $ show world
  let players = keys $ stats world
      actions = fmap (turn $ player world) players
  events <- (fmap catMaybes) $ sequence $ nature : actions
  let newWorld = foldl think world events
  return (events, newWorld)
  where
    turn player name =
      if player /= name then
        npc name
      else
        pc name

describe :: Event -> Maybe String
describe Earthquake =
  Just "Rumble..."
describe Rain =
  Just "It starts raining."
describe (PlayerAction player action) =
  Just $ "[" ++ player ++ "] " ++ (show action)
describe _ = Nothing 

think :: World -> Event -> World
think world (PlayerAction player (Move d)) =
  world {
    stats = adjustWithKey (move d) player (stats world)
  }
think world (PlayerAction player Swing) =
  let opponent = head $ filter (/= player) $ keys $ stats world
  in world {
    stats = adjustWithKey swing opponent (stats world)
  }
think world WorldStop = world { running = False }
think world _ = world

move :: Dir -> Player -> Stats -> Stats
move U _ s = s { x = (x s) + 1 }
move D _ s = s { x = (x s) - 1 }
move R _ s = s { y = (y s) + 1 }
move L _ s = s { y = (y s) - 1 }

swing :: Player -> Stats -> Stats
swing _ s = s { life = (life s) - 1 }

pc :: Channel m => Player -> m (Maybe Event)
pc player = runMaybeT $ do
  command <- MaybeT $ pcCmd player
  MaybeT $ processCommand player command

processCommand :: Channel m => Player -> Cmd -> m (Maybe Event)
processCommand player (Cmd action) = return $ Just (PlayerAction player action)
processCommand player Help = do
  display "Commands: (w) up, (s) down, (a) left, (d) right, (x) swing, (?) help, (x) quit"
  return Nothing
processCommand player Quit = return $ Just WorldStop
processCommand player (Other o) = do
  display $ "Bad command: " ++ o
  return Nothing

--
-- IO bindings
--

instance Channel IO where
  pcCmd   = pcCmdIo
  npc     = npcIo
  nature  = natureIo
  display = putStrLn

pcCmdIo :: Player -> IO (Maybe Cmd)
pcCmdIo player = runMaybeT $ do
  input <- MaybeT $ getInput player
  return $ parseCommand input

getInput :: Player -> IO (Maybe String)
getInput player = do
  putStr $ player ++ "> "
  c <- timeout 3000000 getChar
  putStrLn ""
  return $ (flip (:) []) <$> c

parseCommand :: String -> Cmd
parseCommand s = case s of
  "w" -> Cmd $ Move U
  "s" -> Cmd $ Move D
  "a" -> Cmd $ Move L
  "d" -> Cmd $ Move R
  "x" -> Cmd Swing
  "?" -> Help
  "q" -> Quit
  _   -> Other s

npcIo :: Player -> IO (Maybe Event)
npcIo player = do
  dice <- rollDice
  return $ (PlayerAction player)
    <$> case dice of
      1 -> Just $ Move U
      2 -> Just $ Move D
      3 -> Just $ Move L
      4 -> Just $ Move R
      5 -> Just Swing
      _ -> Nothing

rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 20))

natureIo :: IO (Maybe Event)
natureIo = do
  dice <- rollDice
  return $ case dice of
    1 -> Just Rain
    2 -> Just Earthquake
    _ -> Nothing

--
-- Testing
--

data MockState = MockState {
    events :: MockEvents
  , msgs   :: [String]
  }
  deriving (Eq, Show)

data MockEvents = MockEvents {
    cmds      :: [Maybe Cmd]
  , npcEvents :: [Maybe Event]
  , natEvents :: [Maybe Event]
  }
  deriving (Eq, Show)

type MockChannel = State MockState

instance Channel MockChannel where
  pcCmd _ = do
    state <- get
    let es = events state
    put state { events = es { cmds = tail (cmds es) } }
    return $ head (cmds es)
  npc   _ = do
    state <- get
    let es = events state
    put state { events = es { npcEvents = tail (npcEvents es) } }
    return $ head (npcEvents es)
  nature  = do
    state <- get
    let es = events state
    put state { events = es { natEvents = tail (natEvents es) } }
    return $ head (natEvents es)
  display d = do
    state <- get
    put state { msgs = (msgs state) ++ [d] }
    return ()

up   = Just (Cmd $ Move U)
down = Just (PlayerAction "ogre" $ Move D)
quit = Just Quit
help = Just Help

start =
  MockState {
    events = MockEvents {
      cmds      = [up, quit],
      npcEvents = [down, Nothing],
      natEvents = [Nothing, Nothing]
    }
  , msgs = []
  }
myLoop   = loop initialWorld :: MockChannel ()
final    = runState myLoop start

myNext   = next initialWorld :: MockChannel ([Event], World)
justQuit = MockState {
    events = MockEvents {
      cmds      = [quit],
      npcEvents = [Nothing],
      natEvents = [Nothing]
    }
  , msgs = []
  }
n = runState myNext justQuit
justHelp = MockState {
    events = MockEvents {
      cmds      = [help],
      npcEvents = [Nothing],
      natEvents = [Nothing]
    }
  , msgs = []
  }
h = runState myNext justHelp
