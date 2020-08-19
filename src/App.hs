module App where

import           Control.Monad             (Monad (..), forM, forM_)
import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.List                 (unlines)
import           Data.Map                  (Map, adjustWithKey, fromList, keys,
                                            lookup)
import qualified Data.Map                  (filter)
import           Data.Maybe                (Maybe (..), catMaybes, fromJust,
                                            fromMaybe, maybe)
import           Prelude                   hiding (lookup)

type Player = String

data Stats = Stats {
    x    :: Int
  , y    :: Int
  , life :: Int
  }
  deriving (Show, Eq)

data Game = Game
  { world  :: World
  , quests :: [Quest]
  }
  deriving (Show, Eq)

data World = World
  { player  :: Player
  , stats   :: Map Player Stats
  , running :: Bool
  }
  deriving (Show, Eq)

data Event =
    Moved Player Dir
  | Swinged Player Player
  | QuestCompleted Quest
  | Missed Player
  | PlayerDied Player
  | Nature NatEvent
  | WorldStop
  | BadCommandOrHelp
  deriving (Eq, Show)

data Quest = Quest
  { name :: String
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

data PlayerCmd = PlayerCmd
  { cmd        :: Cmd
  , playerName :: Player
  }
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

game :: Channel m => Game -> m Game
game g = do
  display "Starting game (press '?' for help)"
  intro (world g)
  g' <- loop g
  display "Bye!"
  return g'

intro :: Channel m => World -> m ()
intro world = do
  case descOthers world of
    Just d  -> display d
    Nothing -> return ()

loop :: Channel m => Game -> m Game
loop g@(Game world quests) = do
  maybeCommands <- sequence $ fmap (turn world) $ players world
  maybeNature   <- nature
  let (events, g'@(Game newWorld newQuests)) = runState (next' maybeCommands maybeNature) g
  if running newWorld
    then do
      displayJust . descOthers $ newWorld
      forM_ events (displayJust . descEvent)
      loop $ Game newWorld newQuests
    else
      return $ Game newWorld newQuests
  where
    displayJust =
      maybe (return ()) display

turn :: Channel m => World -> Player -> m (Maybe PlayerCmd)
turn world name = runMaybeT $ do
  cmd <- MaybeT $ if player world == name then pcCmd else npcCmd
  return $ PlayerCmd cmd name

next' :: [Maybe PlayerCmd] -> Maybe NatEvent -> State Game [Event]
next' maybeCommands maybeNature = do
  g <- get
  let (events, g') = next g maybeCommands maybeNature
  put g'
  return events

next :: Game -> [Maybe PlayerCmd] -> Maybe NatEvent -> ([Event], Game)
next (Game world quests) maybeCommands maybeNature =
  let natureEvent      = Nature <$> maybeNature
      playerEvents     = (fmap . fmap) cmdToEvent maybeCommands
      events           = catMaybes $ natureEvent : playerEvents
      (newWorld, dead) = removeDead $ foldl think world $ events
      (compl, act)     = processQuests newWorld quests
      deadEvents       = PlayerDied <$> dead
      questEvents      = QuestCompleted <$> compl
  in (events ++ deadEvents ++ questEvents, (Game newWorld act))
  where
    cmdToEvent :: PlayerCmd -> Event
    cmdToEvent pc = processCommand world (playerName pc) (cmd pc)

processQuests :: World -> [Quest] -> ([Quest], [Quest])
processQuests world quests =
  let completed = filter isDone quests
      active    = filter (not . isDone) quests
  in (completed, active)
  where
    isDone :: Quest -> Bool
    isDone = flip done $ world

processCommand :: World -> Player -> Cmd -> Event
processCommand world player (Move dir) =
  Moved player dir
processCommand world player Swing =
  let opponents = filter (/= player) $ players world
      inRange   = filter (closeToPlayer world player) opponents
  in
    if length inRange > 0
      then
        let opponent = head inRange
        in Swinged player opponent
      else
        Missed player
  where
    closeToPlayer = playerIsAtDistance 2
processCommand world player Help =
  BadCommandOrHelp
processCommand world player (Other o) =
  BadCommandOrHelp
processCommand world player Quit =
  WorldStop

removeDead :: World -> (World, [Player])
removeDead world =
  let dead     = filter (isDead world) $ players world
      newWorld = world { stats = Data.Map.filter (\s -> life s > 0) (stats world) }
  in (newWorld, dead)

isDead :: World -> Player -> Bool
isDead world player =
  let stat = lookup player $ stats world
  in case stat of
    Nothing -> False
    Just s  -> life s == 0

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

playerIsAtDistance :: Integer -> World -> Player -> Player -> Bool
playerIsAtDistance lim world player opponent =
  let dist = distance world player opponent
  in case dist of
    Nothing -> False
    Just d  -> d <= lim

distance :: World -> Player -> Player -> Maybe Integer
distance world player opponent = do
  oppStats <- statsFor world opponent
  playerStats <- statsFor world player
  let xo = x oppStats
      yo = y oppStats
      xp = x playerStats
      yp = y playerStats
      dist = (xo - xp)^2 + (yo - yp)^2
  return $ round . sqrt . fromIntegral $ dist

players :: World -> [Player]
players world = keys $ stats world

statsFor :: World -> Player -> Maybe Stats
statsFor world player = lookup player $ stats world

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
descEvent (PlayerDied died) =
  Just $ died ++ " is dead."
descEvent BadCommandOrHelp =
  Just "Commands: (w) up, (s) down, (a) left, (d) right, (x) swing, (?) help, (x) quit"
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

