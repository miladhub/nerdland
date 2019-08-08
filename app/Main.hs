module Main where

import Lib
import Data.Map (Map, fromList, keys, adjustWithKey)
import System.Random
import Control.Monad
import Data.Maybe (catMaybes)

type Player = String

data Stats = Stats {
    x     :: Int
  , y     :: Int
  , life  :: Int
  }
  deriving (Show, Eq)

data World = World {
    stats :: Map Player Stats
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
  deriving (Eq, Show)

data Dir = U | D | L | R
  deriving (Eq, Show)

initialStats = Stats {
    x = 10
  , y = 10
  , life = 10
  }

initialWorld = World {
  stats = fromList $ (,)
    <$> ["milad", "ogre"] 
    <*> [initialStats]
}

main :: IO ()
main = loop initialWorld "milad"

loop :: World -> Player -> IO ()
loop world player = do
  putStrLn $ show world
  let players = keys $ stats world
      actions = fmap (turn player) players
  events <- (fmap catMaybes) $ sequence $ nature : actions
  forM_ events describe
  let newWorld = foldl think world events
  loop newWorld player
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
think world _ = world

move :: Dir -> Player -> Stats -> Stats
move U _ s = s { x = (x s) + 1 }
move D _ s = s { x = (x s) - 1 }
move R _ s = s { y = (y s) + 1 }
move L _ s = s { y = (y s) - 1 }

swing :: Player -> Stats -> Stats
swing _ s = s { life = (life s) - 1 }

pc :: Player -> IO (Maybe Event)
pc player = do
  putStr $ player ++ "> "
  a <- getLine
  return $ (PlayerAction player)
    <$> case a of
      "up"    -> Just $ Move U
      "down"  -> Just $ Move D
      "left"  -> Just $ Move L
      "right" -> Just $ Move R
      "swing" -> Just Swing
      _       -> Nothing

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
