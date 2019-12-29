module App where

import Data.Map (Map, fromList, keys, adjustWithKey, lookup)
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
  | Missed Player
  | Nature NatEvent
  | WorldStop
  deriving (Eq, Show)

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

game :: Channel m => World -> m World
game world = do
  intro world
  final <- loop world
  bye
  return final

intro :: Channel m => World -> m ()
intro world = do
  display "Starting game (press '?' for help)"
  case descOthers world of
    Just d  -> display d
    Nothing -> return ()

bye :: Channel m => m ()
bye = do
  display "Bye!"

loop :: Channel m => World -> m World
loop world = do
  (events, newWorld) <- next world
  if running newWorld
    then do
      forM_ events (showDesc world newWorld)
      loop newWorld
    else
      return newWorld
  where
    showDesc world newWorld =
      maybe (return ()) display . (descrEvent world newWorld)

next :: Channel m => World -> m ([Event], World)
next world = do
  let players   = keys $ stats world
      actions   = fmap (turn world) players
      natEvents = (fmap . fmap) Nature nature
  events <- (fmap catMaybes) $ sequence $ natEvents : actions
  let newWorld = foldl think world events
  return (events, newWorld)

turn :: Channel m => World -> Player -> m (Maybe Event)
turn world name =
  let cmd = if player world == name then pcCmd else npcCmd
  in runMaybeT $ do
    command <- MaybeT $ cmd
    MaybeT $ processCommand world name command

descrEvent :: World -> World -> Event -> Maybe String
descrEvent _ _ (Nature Earthquake) =
  Just "Rumble..."
descrEvent _ _ (Nature Rain) =
  Just "It starts raining."
descrEvent world newWorld (Moved moving dir) =
  case desc of
    [] -> Nothing
    d  -> Just $ unlines d
  where
    desc           = sees <> lost <> dist
    p              = player newWorld
    players        = keys $ stats newWorld
    opponents      = filter (/= p) players
    inRange        = filter isNew opponents
    lostRange      = filter isLost opponents
    visibles       = filter visible opponents
    isNew other    = (canSee newWorld other) && not (canSee world other)
    isLost other   = (canSee world other) && not (canSee newWorld other)
    visible other  = canSee newWorld other
    canSee w other = playerIsAtDistance 10 w (player w) other
    sees           = fmap (\np -> "You see " ++ np) inRange
    lost           = fmap (\np -> "You lost sight of " ++ np) lostRange
    dist           = fmap (\np -> np ++ ": " ++ fromJust (show <$> (distance newWorld p np))) visibles
descrEvent _ _ (Swinged player opponent) =
  Just $ "[" ++ player ++ "] swinged " ++ opponent ++ "!"
descrEvent _ _ (Missed player) = 
  Just $ "[" ++ player ++ "] missed!"
descrEvent _ _ _ = Nothing

descOthers :: World -> Maybe String
descOthers newWorld =
  if length sees > 0
    then Just $ unlines sees
  else
    Nothing
  where
    p              = player newWorld
    players        = keys $ stats newWorld
    opponents      = filter (/= p) players
    inRange        = filter isNew opponents
    isNew other    = canSee newWorld other
    canSee w other = playerIsAtDistance 10 w (player w) other
    sees           = fmap (\np -> "You see " ++ np) inRange

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

playerIsAtDistance :: Int -> World -> Player -> Player -> Bool
playerIsAtDistance lim world player opponent = fromMaybe False $ do
  oppStats <- lookup opponent $ stats world
  playerStats <- lookup player $ stats world
  let xo = x oppStats
      yo = y oppStats
      xp = x playerStats
      yp = y playerStats
      dist = (xo - xp)^2 + (yo - yp)^2
  return (dist <= lim^2)

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
