module Main where

import Data.Map (Map, fromList, keys, adjustWithKey)
import System.Random (getStdRandom, randomR)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import System.Timeout (timeout)
import Control.Monad.Trans.Maybe (MaybeT(..))

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

loop :: World -> IO ()
loop world = do
  putStrLn $ show world
  let players = keys $ stats world
      actions = fmap (turn $ player world) players
  events <- (fmap catMaybes) $ sequence $ nature : actions
  let newWorld = foldl think world events
  if running newWorld
    then do
      forM_ events describe
      loop newWorld
    else
      return ()
  where
    turn player name =
      if player /= name then
        npc name
      else
        pc name

describe :: Event -> IO ()
describe Earthquake =
  putStrLn "Rumble..."
describe Rain =
  putStrLn "It starts raining."
describe (PlayerAction player action) =
  putStrLn $ "[" ++ player ++ "] " ++ (show action)
describe _ = return ()

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

pc :: Player -> IO (Maybe Event)
pc player = runMaybeT $ do
  input <- MaybeT $ getInput player
  command <- return $ parseCommand input
  MaybeT $ processCommand player command

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

processCommand :: Player -> Cmd -> IO (Maybe Event)
processCommand player (Cmd action) = return $ Just (PlayerAction player action)
processCommand player Help = do
  putStrLn "Commands: (w) up, (s) down, (a) left, (d) right, (x) swing, (?) help, (x) quit"
  return Nothing
processCommand player Quit = return $ Just WorldStop
processCommand player (Other o) = do
  putStrLn $ "Bad command: " ++ o
  return Nothing

npc :: Player -> IO (Maybe Event)
npc player = do
  dice <- rollDice
  return $ (PlayerAction player)
    <$> case dice of
      1 -> Just $ Move U
      2 -> Just $ Move D
      3 -> Just $ Move L
      4 -> Just $ Move R
      5 -> Just Swing
      _ -> Nothing

nature :: IO (Maybe Event)
nature = do
  dice <- rollDice
  return $ case dice of
    1 -> Just Rain
    2 -> Just Earthquake
    _ -> Nothing

rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 20))
