module Main where

import App
import IoBindings
import Data.Map (fromList)
import System.IO

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
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  game initialWorld

