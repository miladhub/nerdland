module App (
    Player
  , Stats(..)
  , World(..)
  , Action(..)
  , Event(..)
  , Cmd(..)
  , Dir(..)
  , Channel(..)
  , loop
  , next
  ) where

import Data.Map (Map, fromList, keys, adjustWithKey, lookup)
import Control.Monad (Monad(..), forM_)
import Data.Maybe (Maybe(..), catMaybes, maybe, fromMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Prelude hiding (lookup)

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

--
-- Internals
--

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
  let opponents = filter (/= player) $ keys $ stats world
      inRange   = filter (closeToPlayer world player) opponents
  in
    if (length inRange > 0)
      then
        let opponent = head inRange
        in world {
          stats = adjustWithKey swing opponent (stats world)
        }
      else
        world
  
think world WorldStop = world { running = False }
think world _ = world

closeToPlayer :: World -> Player -> Player -> Bool
closeToPlayer world player opponent = fromMaybe False $ do
  oppStats <- lookup opponent $ stats world
  playerStats <- lookup player $ stats world
  let xo = x oppStats
      yo = y oppStats
      xp = x playerStats
      yp = y playerStats
      dist = (xo - xp)^2 + (yo - yp)^2
  return (dist <= 2)

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


