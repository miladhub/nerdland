module App where

import Data.Map (Map, fromList, keys, adjustWithKey, lookup)
import qualified Data.Map (filter) 
import Control.Monad (Monad(..), forM_, forM)
import Data.Maybe (Maybe(..), catMaybes, maybe, fromMaybe, fromJust)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Prelude hiding (lookup)
import Data.List (unlines)

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

data Event =
    Moved Player Dir
  | Swinged Player Player
  | QuestCompleted Quest
  | Missed Player
  | Nature NatEvent
  | WorldStop
  deriving (Eq, Show)

data Quest = Quest
  {
    name :: String
  , done :: World -> Bool
  }

instance Show Quest where
  show = name

instance Eq Quest where
  q1 == q2 = (name q1) == (name q2)

data NatEvent =
    Rain
  | Earthquake
  deriving (Eq, Show)

data Cmd =
    Move Dir
  | Swing
  | Help
  | Quit
  | Other String
  deriving (Eq, Show)

data Dir = U | D | L | R
  deriving (Eq, Show)

class Monad m => Channel m where
  pcCmd   :: m (Maybe Cmd)
  npcCmd  :: m (Maybe Cmd)
  nature  :: m (Maybe NatEvent)
  display :: String -> m ()

game :: Channel m => World -> [Quest] -> m World
game world quests = do
  display "Starting game (press '?' for help)"
  intro world
  final <- loop world quests
  display "Bye!"
  return final

intro :: Channel m => World -> m ()
intro world = do
  case descOthers world of
    Just d  -> display d
    Nothing -> return ()

loop :: Channel m => World -> [Quest] -> m World
loop world quests = do
  (events, newWorld, newQuests) <- next world quests
  if running newWorld
    then do
      displayJust . descOthers $ newWorld
      forM_ events (displayJust . descEvent)
      loop newWorld newQuests
    else
      return newWorld
  where
    displayJust =
      maybe (return ()) display

next :: Channel m => World -> [Quest] -> m ([Event], World, [Quest])
next world quests = do
  let actions     = fmap (turn world) $ players world
      natEvent    = (fmap . fmap) Nature nature
      complQuests = QuestCompleted <$> filter isDone quests
  events <- (fmap catMaybes) $ sequence $ natEvent : actions
  let newWorld = foldl think world $ events ++ complQuests
  final <- processDead newWorld
  return (events ++ complQuests, final, filter (not . isDone) quests)
  where
    isDone :: Quest -> Bool
    isDone = flip done $ world

turn :: Channel m => World -> Player -> m (Maybe Event)
turn world name =
  let cmd = if player world == name then pcCmd else npcCmd
  in runMaybeT $ do
    command <- MaybeT $ cmd
    MaybeT $ processCommand world name command

processDead :: Channel m => World -> m World
processDead world = do
  let dead = filter (isDead world) $ players world
  forM_ dead $
    \d -> display $ d ++ " is dead."
  return world { stats = Data.Map.filter (\s -> life s > 0) (stats world) }

isDead :: World -> Player -> Bool
isDead world player =
  let stat = lookup player $ stats world
  in case stat of
    Nothing -> False
    Just s  -> life s == 0

descEvent :: Event -> Maybe String
descEvent (Nature Earthquake) =
  Just "Rumble..."
descEvent (Nature Rain) =
  Just "It starts raining."
descEvent (Swinged player opponent) =
  Just $ "[" ++ player ++ "] swinged " ++ opponent ++ "!"
descEvent (Missed player) = 
  Just $ "[" ++ player ++ "] missed!"
descEvent (QuestCompleted quest) =
  Just $ "Quest " ++ (name quest) ++ " completed!"
descEvent _ = Nothing

descOthers :: World -> Maybe String
descOthers world =
  case dist of
    [] -> Nothing
    d  -> Just $ unlines d
  where
    dist          = fmap (\np -> np ++ ": " ++ fromJust
      (show <$> (distance world p np)))
      visibles
    p             = player world
    visibles      = filter visible opponents
    visible other = playerIsAtDistance 10 world p other
    opponents     = filter (/= p) $ players world

players :: World -> [Player]
players world = keys $ stats world

think :: World -> Event -> World
think world (Moved player dir) =
  world {
    stats = adjustWithKey (move dir) player (stats world)
  }
think world (Swinged player opponent) =
  world {
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

processCommand :: Channel m => World -> Player -> Cmd -> m (Maybe Event)
processCommand world player (Move dir) = return $ Just (Moved player dir)
processCommand world player Swing =
  let opponents = filter (/= player) $ keys $ stats world
      inRange   = filter (closeToPlayer world player) opponents
  in
    if length inRange > 0
      then do
        let opponent = head inRange
        return $ Just $ Swinged player opponent
      else
        return $ Just $ Missed player
processCommand world player Help = do
  display "Commands: (w) up, (s) down, (a) left, (d) right, (x) swing, (?) help, (x) quit"
  return Nothing
processCommand world player Quit = return $ Just WorldStop
processCommand world player (Other o) = do
  display $ "Bad command: " ++ o
  return Nothing

closeToPlayer :: World -> Player -> Player -> Bool
closeToPlayer = playerIsAtDistance 2

playerIsAtDistance :: Integer -> World -> Player -> Player -> Bool
playerIsAtDistance lim world player opponent =
  let dist = distance world player opponent
  in case dist of
    Nothing -> False
    Just d -> d <= lim

distance :: World -> Player -> Player -> Maybe Integer
distance world player opponent = do
  oppStats <- lookup opponent $ stats world
  playerStats <- lookup player $ stats world
  let xo = x oppStats
      yo = y oppStats
      xp = x playerStats
      yp = y playerStats
      dist = (xo - xp)^2 + (yo - yp)^2
  return $ round . sqrt . fromIntegral $ dist
