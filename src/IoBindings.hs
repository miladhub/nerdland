module IoBindings (Channel) where

import App
import System.Random (getStdRandom, randomR)
import System.Timeout (timeout)
import Control.Monad.Trans.Maybe (MaybeT(..))

instance Channel IO where
  pcCmd   = pcCmdIo
  npcCmd  = npcIo
  nature  = natureIo
  display = putStrLn

pcCmdIo :: IO (Maybe Cmd)
pcCmdIo = runMaybeT $ do
  input <- MaybeT $ getInput
  return $ parseCommand input

getInput :: IO (Maybe Char)
getInput = do
  putStr "> "
  c <- timeout 3000000 getChar
  return c

parseCommand :: Char -> Cmd
parseCommand c = case c of
  'w' -> Move U
  's' -> Move D
  'a' -> Move L
  'd' -> Move R
  'x' -> Swing
  '?' -> Help
  'q' -> Quit
  _   -> Other [c]

npcIo :: IO (Maybe Cmd)
npcIo = do
  dice <- rollDice
  return $ case dice of
    1 -> Just $ Move U
    2 -> Just $ Move D
    3 -> Just $ Move L
    4 -> Just $ Move R
    5 -> Just Swing
    _ -> Nothing

rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 20))

natureIo :: IO (Maybe Event)
natureIo = do
  dice <- rollDice
  return $ case dice of
    1 -> Just Rain
    2 -> Just Earthquake
    _ -> Nothing
    
