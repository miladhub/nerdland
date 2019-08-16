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
    stats   :: Map Player Stats
  , stopped :: Bool
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
    x = 10
  , y = 10
  , life = 10
  }

initialWorld = World {
    stats    = fromList $ (,)
      <$> ["milad", "ogre"]
      <*> [initialStats]
  , stopped  = False
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
  if stopped newWorld
    then putStrLn "Stopping..."
    else loop newWorld player
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
think world WorldStop = world { stopped = True }
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
  l <- timeout 3000000 getLine
  case l of
    (Just i) -> return (Just i)
    Nothing -> do
      putStrLn "(aborted)"
      return Nothing

parseCommand :: String -> Cmd
parseCommand s = case s of
  "up"    -> Cmd $ Move U
  "down"  -> Cmd $ Move D
  "left"  -> Cmd $ Move L
  "right" -> Cmd $ Move R
  "swing" -> Cmd Swing
  "help"  -> Help
  "quit"  -> Quit
  _       -> Other s

processCommand :: Player -> Cmd -> IO (Maybe Event)
processCommand player (Cmd action) = return $ Just (PlayerAction player action)
processCommand player Help = do
  putStrLn "Commands: up, down, left, right, swing, help, quit"
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
