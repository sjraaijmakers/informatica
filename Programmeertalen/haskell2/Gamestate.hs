-----------------------------------------------------------------------------
-- | Programmeertalen Haskel Week 2
--
-- Author   : David Veenstra
-- Year     : 2014
--
-- This file contains data structured that are used as the state for the players
-- and the game itself.
-----------------------------------------------------------------------------

module Gamestate where

import qualified Data.Map.Lazy as Map

import MM

-- | This data structure contains the state of a given player
-- Different constructors are used to make a distinction
-- between the state of a real player, and that of a computer.
--
-- Player secret history:
--      The state of a real player. secret is the combination that the player must gues.
--      history contains every move that the player has made.
--
-- CPU secret turn moves:
--      The state of a computer player. secret is the combination that the player must gues.
--      moves contain the moves that the computer will make.
--      turn indicates the index of the next move that the computer will make.
data PlayerState = Player Pattern [Pattern]
                 | CPU Pattern Int [Pattern]
                 deriving (Show, Eq)

-- | A game event that is generated during the game.
--
-- UpdatePlayer newState move:
--      A player has made a move, and its new state is newState.
--
-- GameOver secret:
--      A player has won the game, and secret is the secret of the winner.
--
-- PrintHistory:
--      A player has made a request to print its history.
--
-- Quit:
--      A player has mad a request to quit the game.
--
-- Help:
--      A player has made a request for the help menu.
data GameEvent = UpdatePlayer PlayerState Pattern
               | GameOver Pattern
               | PrintHistory
               | Quit
               | Help
               deriving (Show, Eq)
