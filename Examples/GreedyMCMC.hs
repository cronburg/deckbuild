module Examples.GreedyMCMC where

import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Types
import Game.Sample.Sample
import Game.Sample.Hakaru
import Examples.Base
import Examples.GreedySampler

--import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution
import Language.Hakaru.Mixture (toList, Prob)

import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

import System.IO.Unsafe (unsafePerformIO)

greedyModel :: Measure Int
greedyModel = do
  param0 <- uncnd $ uniform 0 1
  let param1 = 1 - param0
  g <- runGreedy (param0,param1)
  return $ turn g

-- completely unconditioned - MCMC sampling from a uniform parameter prior
--main n = empiricalMeasure n greedyModel []

  --do
  --mixture <- empiricalMeasure 10 greedyModel []
  --return $ toList mixture
  --samples <- mcmc greedyModel []
  --return $ take n samples

