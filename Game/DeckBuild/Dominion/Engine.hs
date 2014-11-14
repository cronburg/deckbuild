{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Engine where
--import Data.List
--import Data.Default.Class
--import Control.Lens

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Types
import Game.Sample
import Control.Monad.State

-- TODO: Run actHeuristic of player #1:
actionPhase :: forall (m :: * -> *). MonadState Game m => m ()
actionPhase = do
    return ()

buyPhase :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
buyPhase = do
  g <- get
  -- If no more buys, return, else ask player what card they want
  if (numBuys . p1) g == 0 then return () else do
    c' <- liftIO $ ((buyHeuristic . p1) g) g
    decrBuys 1
    -- Not really necessary, but doing it for safety since decrBuys affects the
    -- state, and I might e.g. change how canBuy works in the future:
    g' <- get
    case c' of
      -- If you can buy the card, buy it, else ignore the ignorant player trying to steal cards:
      Just c  -> if canBuy g c then buyCard c >> buyPhase else return ()
      -- Player said "I don't want anything" - end the buy phase:
      Nothing -> return ()

-- Executes all phases of player #1's turn:
takeTurn :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
takeTurn = do
    actionPhase
    playMoney
    buyPhase
    discard
    draw 5
    g <- get
    put $ g { p1 = (p1 g) { numActions=1, numBuys=1, amtMoney=0 },
              turn = (turn g) + 1}

shuffleDrawSwap :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
shuffleDrawSwap = do
    shuffleCards
    draw 5
    swapPlayers

runGameLoop :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
runGameLoop = do
    takeTurn
    swapPlayers
    g <- get
    --if ((turn g) == 100) then return ()
    over <- gameOver
    if over then do return ()
    else do runGameLoop

-- Run the game:
runGame :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m ()
runGame = do
    shuffleDrawSwap
    shuffleDrawSwap
    runGameLoop

