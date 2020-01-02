module Main where

import App
import IoBindings
import Data.Map (fromList, toList)
import System.IO
import System.Environment (getArgs)
import Data.Map (Map, fromList)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  args <- getArgs
  let file = if (length args > 0) then args !! 0 else "world.txt"
  putStrLn $ "Using file " ++ file
  world <- readWorld file
  final <- game world [killAll]
  save final "save.txt"

killAll :: Quest
killAll = Quest
  {
    name = "Kill All"
  , done = \world ->
      let players = toList $ stats world
      in length players == 1
  }

readWorld :: String -> IO World
readWorld file = do
  world <- readFile file
  return $ World
    {
      player = readPlayer world
    , stats = readStats world
    , running = True
    }

readPlayer :: String -> String
readPlayer world =
  let all = lines world
      first = words $ all !! 0
  in first !! 0

readStats :: String -> Map Player Stats
readStats world =
  let stats = filter (\l -> length l == 4) $ words <$> lines world
  in fromList $ readStat <$> stats

readStat :: [String] -> (Player, Stats)
readStat ws =
  let player = ws !! 0
      x      = read $ ws !! 1
      y      = read $ ws !! 2
      life   = read $ ws !! 3
  in (player, Stats x y life)

save :: World -> String -> IO ()
save world file =
  writeFile file $ writeStats world

writeStats :: World -> String
writeStats world =
  let players     = toList $ stats world
      pc          = filter (\(p, _) -> p == player world) players
      npcs        = filter (\(p, _) -> p /= player world) players
      toRow (p,s) = unwords [p, show $ x s, show $ y s, show $ life s]
      rows        = fmap toRow $ pc ++ npcs
  in unlines rows
