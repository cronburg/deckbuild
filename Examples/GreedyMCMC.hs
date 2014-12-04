module Examples.GreedyMCMC where

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample.Sample
import Examples.Base
import Examples.GreedySampler

import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution
import Language.Hakaru.Mixture (toList)

import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

import System.IO.Unsafe (unsafePerformIO)

greedyModel :: IS.Measure Int
greedyModel = do
  param0 <- uncnd $ uniform 0 1
  let param1 = 1 - param0
  runGreedy (param0,param1)
  g <- get
  return $ turn g

main n = do
  mixture <- IS.empiricalMeasure 10 greedyModel []
  return $ toList mixture
  --samples <- mcmc greedyModel []
  --return $ take n samples

