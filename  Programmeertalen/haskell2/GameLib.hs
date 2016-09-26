-----------------------------------------------------------------------------
-- | Programmeertalen Haskel Week 2
--
-- Author   : David Veenstra
-- Year     : 2014
--
-- This file implements the GameM monad and functions that can be used
-- to get and modify the game state.
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GameLib where

import qualified Data.Map.Lazy as Map
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.State.Lazy hiding (get, gets, put)
import Control.Applicative
import System.Random

import Gamestate
import MM

data GameState = GameState {turns::[Int], playersState::Map.Map Int PlayerState}
newtype GameM a = GameM {gameState::StateT GameState IO a}
     deriving (Monad, MonadIO, MonadState GameState)

instance Functor GameM where
  fmap = liftM

instance Applicative GameM where
    pure = return
    (<*>) = ap

-- | This action retrieves the id of the current player.
currentPlayer :: GameM Int
currentPlayer = do
  (current: _) <- gets turns
  return current


-- | This action gives the turn to the next player.
-- For example, for two players, starting with player 1:
--     do before <- currentPlayer
--        nextPlayer
--        after <- currentPlayer
--
-- before will have value 1, and after will have value 2.
nextPlayer :: GameM ()
nextPlayer = do
  (_ : tail') <- gets turns
  gameState <- get
  put (gameState{turns=tail'})


-- | This action sets the state of the player with the given id.
-- The first player, Player 1, has id 1.
setPlayer :: Int -> PlayerState -> GameM ()
setPlayer id playerState = do
  state <- get
  let playerMap = playersState state
      newMap = Map.insert id playerState playerMap
  put $ state{playersState=newMap}


-- | This action retrieves the player state of the player
-- with the given id.
-- The first player, Player 1, has id 1.
getPlayer :: Int -> GameM PlayerState
getPlayer id = do
  playerMap <- gets playersState
  return $ playerMap Map.! id

-- | This action returns the number of players.
getNumPlayer :: GameM Int
getNumPlayer = do
  playerMap <- gets playersState
  return $ Map.size playerMap


-- | This action generates a list of random Patterns
randomPatterns :: Int -> IO [Pattern]
randomPatterns n = do
  g <- getStdGen
  let colorsInt = take (n * 4) (randomRs (fromEnum (minBound::Color), fromEnum (maxBound::Color)) g)
      colors = map toEnum colorsInt
      groupBy4 list = case pattern of
                        [] -> []
                        _  -> pattern : groupBy4 rest
          where (pattern, rest) = splitAt 4 list
  return $ groupBy4 colors


-- | This function executes a game action.
-- The initial state of each player must be provided.
runGame :: GameM a -> [PlayerState] -> IO ()
runGame gameAction players =
  let playerIds = [1.. (length players)]
      state = GameState (cycle playerIds) $ Map.fromList (zip playerIds players)
  in execStateT (gameState gameAction) state >> return ()
