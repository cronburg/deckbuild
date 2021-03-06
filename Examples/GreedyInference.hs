{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances,
    FlexibleContexts, KindSignatures, ScopedTypeVariables #-}
module Examples.GreedyInference where

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample.Sample
import Examples.Base
import Examples.BaseQuote

import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

import System.IO.Unsafe (unsafePerformIO)
import Data.Dynamic (toDyn)
import Data.Typeable

-- Whether or not player #1 wants to buy a card during this buy phase:
wantToBuy :: Game -> IS.Measure Bool
wantToBuy g = return $ (amtMoney.p1) g > 2

wantCard :: Game -> CardName -> Bool
wantCard g c = elem c [SILVER, GOLD, DUCHY, PROVINCE, VILLAGE, CHANCELLOR]

cardValue :: (Double,Double) -> Game -> CardName -> Double
cardValue ps g c = let (param0,param1) = ps in
  case ((amtMoney.p1) g, c) of
    (3,SILVER)     -> 2.0
    (3,VILLAGE)    -> param0
    (3,CHANCELLOR) -> param1
    (4,c)          -> cardValue ps (g { p1 = (p1 g) { amtMoney=3 } }) c -- TODO: field 'setters':
    (5,c)          -> cardValue ps (g { p1 = (p1 g) { amtMoney=3 } }) c
    (6,GOLD)       -> 3.0
    (7,c)          -> cardValue ps (g { p1 = (p1 g) { amtMoney=6 } }) c
    (_,PROVINCE)   -> 1.0
    otherwise      -> 0.0
  
sampleBuy :: (Double,Double) -> Game -> IS.Measure CardName
sampleBuy ps g = do
    card <- uncnd $ categorical $
      [(c, cardValue ps g c) |                   -- Categorical value
        c <- ((map fst) . piles . supply) g,     -- CardNames in supply
        (cardValue ps g c) > 0.0 && canBuy g c]  -- Buy conditions
    return card

greedyBuy :: (Double,Double) -> Game -> IO (Maybe CardName)
greedyBuy ps g = do
  wantACard <- sample1 (wantToBuy g) []
  if wantACard then do
    c <- sample1 (sampleBuy ps g) []
    return $ Just c
  else
    return $ Nothing

greedyAct :: Game -> IO (Maybe CardName)
greedyAct g = do
  let as = filter isAction $ (cards.hand.p1) g
  case length as of
    0 -> return Nothing
    _ -> if elem VILLAGE as
         then return $ Just VILLAGE
         else return $ Just $ maximumBy (comparing cost) as

-- Greedy CHANCELLOR always discards deck
greedyMayPick :: Game -> CardName -> IO (Maybe CardName)
greedyMayPick g c' = return $ case c' of
  CHANCELLOR -> Just COPPER  -- any card triggers a discard deck
  otherwise  -> Nothing

-- Not necessary with only VILLAGE and CHANCELLOR actions:
greedyMustPick :: Game -> CardName -> IO CardName
greedyMustPick g c' = undefined

greedyPlayer ps n = defaultPlayer
  { name = n
  , buyHeuristic = greedyBuy ps
  , actHeuristic = greedyAct
  , mayPick      = greedyMayPick
  , mustPick     = greedyMustPick
  }

greedyGame ps = defaultBaseGame
  { p1 = greedyPlayer ps "Greedy1"
  , p2 = greedyPlayer ps "Greedy2"
  }

runGreedy :: MonadIO m => (Double,Double) -> m Game
runGreedy ps = execStateT runGame $ greedyGame ps

logprob :: MonadIO m => (Double,Double) -> m Double
logprob ps = do
  g <- runGreedy ps
  return $ 0 - (fI $ turn g)

-------------------------------------------------------------------------------
-- Top-level functions
greedyModel :: Measure Double
greedyModel = do
  param0 <- unconditioned $ uniform 0 1 
  let param1 = 1 - param0
  let g = unsafePerformIO $ runGreedy (param0,param1)
  turns <- conditioned $ categorical [(turn g, 1.0)]
  return $ param0

main n = do
  --samples <- IS.empiricalMeasure 10 greedyModel [
  samples <- mcmc greedyModel $ [Just (toDyn (Discrete (36 :: Int)))]
  return $ take n samples

