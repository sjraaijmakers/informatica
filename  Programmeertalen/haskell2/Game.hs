-----------------------------------------------------------------------------
-- | Programmeertalen Haskel Week 2
--
-- Student Name   :
-- Student Number :
-----------------------------------------------------------------------------

module Game where

import Data.Char
import Data.List
import Control.Monad
import Control.Monad.IO.Class
import System.Random

import Gamestate
import GameLib

-- Import your work from last week here
import MM

-- | Returns true if the given string is valid command
-- At least the following commands must be recognised:
-- help, quit, history
--
-- For example: map isCommand ["help", "quit", "history", "hello"] == [True, True, True, False]
isCommand :: String -> Bool
isCommand string = undefined

-- | Converts a given command to an event.
-- At least the quit, history and help command must be recognized.
--
-- For example: commandToEvent "quit" == Quit
commandToEvent :: String -> GameEvent
commandToEvent command = undefined

-- | This function trims any leading or trailing whitespace.
-- For example: trim "  Hello    " == "Hello"
trim :: String -> String
trim x = undefined

-- | This function converts a character to a color.
--
-- For example: map charToColor "RGB" = [Red, Green, Blue]
charToColor :: Char -> Color
charToColor c = undefined

-- | This function converts a color to a character.
--
-- For example: map colorToChar [Red, Green, Blue] = "RGB"
colorToChar :: Color -> Char
colorToChar color = undefined

-- | This function prints a string, and then ask the user for input
promptUser :: String -> GameM String
promptUser string = do
  liftIO $ putStrLn string
  liftIO $ getLine

-- | This function creates the initial state for a player played by the computer.
-- The knuth algorithm is used to determine the moves that the computer must make.
-- You may change this to another algorithm.
makeCpu :: Pattern -> PlayerState
makeCpu secret = CPU secret 0 plays
    where plays = knuth_algorithm secret

-- | This function creates an initial state for a human player.
makePlayer :: Pattern -> PlayerState
makePlayer secret = Player secret []

-- | Prints a list of strings, that are joined by the given separator.
printStrings :: String -> [String] -> IO ()
printStrings sep strings = putStrLn $ "Not implemented"


-- | This loop is executed by a given player.
-- The player can be either a real player or a computer.
-- This function must return a GameEvent, in a GameM context.
--
-- For example, the event can be: UpdatePlayer newState move.
-- Where the newState is the new state of the player, and move
-- the master mind move the player has made.
--
-- Different clauses are used for different Constructors.
playerLoop :: PlayerState -> GameM GameEvent
playerLoop ps@(Player secret history) = do
  line <- promptUser "get input:"
  liftIO $ putStrLn "playerLoop is not implemented"
  return Help

playerLoop (CPU secret turn plays) = do
  liftIO $ putStrLn "computer part of playerLoop is not implemented"
  return Help

-- | This main loop of the game. This loops does the playerLoop, and
-- processes the generated event.
gameLoop :: GameM ()
gameLoop = do
  let loop = do
        -- retrieve the id, and the state of the current player
        id <- currentPlayer
        player <- getPlayer id

        -- generate a new event
        event <- playerLoop player

        -- process the different kind of events
        case event of
          UpdatePlayer newPlayerState play -> do
            -- print the move that the current player has made

            -- update the player state of the current player

            -- give the turn to the next player

            -- call loop

            liftIO $ putStrLn "UpdatePlayer event not implemented"

          _ -> do liftIO $ putStrLn (show event ++ " event is not yet implemented")

  -- here you can do any extra work, before starting the loop
  liftIO $ putStrLn "Starting game..."
  loop


main = do
  -- Generate a secret for both players
  secrets <- randomPatterns 2

  -- define the players
  let players :: [PlayerState]
      players = zipWith (\f x -> f x) [makePlayer, makeCpu] secrets

  -- run the game
  runGame gameLoop players
