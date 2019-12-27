module Main where

import App
import IoBindings
import Data.Map (fromList)
import System.IO
import System.Environment (getArgs)
import Data.Map (Map, fromList)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  args <- getArgs
  world <- readWorld "world.txt"
  game world

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
  
