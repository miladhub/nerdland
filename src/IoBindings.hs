module IoBindings (Channel) where

import App
import System.Random (getStdRandom, randomR)
import System.Timeout (timeout)
import Control.Monad.Trans.Maybe (MaybeT(..))

instance Channel IO where
  pcCmd   = pcCmdIo
  npc     = npcIo
  nature  = natureIo
  display = putStrLn

pcCmdIo :: Player -> IO (Maybe Cmd)
pcCmdIo player = runMaybeT $ do
  input <- MaybeT $ getInput player
  return $ parseCommand input

getInput :: Player -> IO (Maybe String)
getInput player = do
  putStr $ player ++ "> "
--  l <- getLine
--  return (Just l)
--  c <- getChar
--  putStrLn ""
--  return $ (flip (:) []) <$> (Just c)
  c <- timeout 3000000 getChar
  putStrLn ""
  return $ (flip (:) []) <$> c

parseCommand :: String -> Cmd
parseCommand s = case s of
  "w" -> Cmd $ Move U
  "s" -> Cmd $ Move D
  "a" -> Cmd $ Move L
  "d" -> Cmd $ Move R
  "x" -> Cmd Swing
  "?" -> Help
  "q" -> Quit
  _   -> Other s

npcIo :: Player -> IO (Maybe Event)
npcIo player = do
  dice <- rollDice
  return $ (PlayerAction player)
    <$> case dice of
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
    
