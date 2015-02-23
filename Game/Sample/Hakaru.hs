{-# LANGUAGE RankNTypes, NoMonomorphismRestriction, BangPatterns #-}
{-# OPTIONS -W #-}
module Game.Sample.Hakaru
  ( Measure
  , sample
  , mcmc
  , cnd
  , uncnd
  ) where

import Language.Hakaru.Types
import Language.Hakaru.Mixture (Prob)
import Language.Hakaru.Mixture (Mixture(..))
import Language.Hakaru.Sampler ()

import qualified System.Random.MWC as MWC
import System.IO.Unsafe
import qualified Data.Map.Strict as M

-- TODO: switch back to Metropolis...
import Language.Hakaru.ImportanceSampler hiding (sample)
--import Language.Hakaru.Metropolis hiding (sample)

--import Data.Typeable (Typeable)

--import Control.Monad.State
--instance (MonadState Game) Measure
--instance MonadIO Measure

--instance MonadIO Measure where
--  liftIO = liftIO

-- Taken from ImportanceSampler.hs in Hakaru (need to modify slightly):
sample :: Measure a -> [Cond] -> IO [(a, Prob)]
sample measure conds = do
  gen <- MWC.createSystemRandom --MWC.create
  unsafeInterleaveIO $ sampleNext gen 
      where once = unMeasure measure conds
            mixToTuple = head . M.toList . unMixture
            sampleNext g = do
              u <- once g
              let x = mixToTuple (finish u)
              xs <- unsafeInterleaveIO $ sampleNext g
              return (x : xs)

{-
-- Metropolis Hastings sampler
sample :: Typeable a => Measure a -> [Cond] -> IO [(a, Double)]
sample prog cds  = do 
  -- TODO: don't recreate this every time we need a sample
  g <- MWC.createSystemRandom
  --g <- MWC.create
  (v, d, llTotal, _, _) <- initialStep prog cds g
  (transition prog cds v d llTotal g) >>= return . map (\ x -> (x,1)) 
-}

mcmc :: Typeable a => Measure a -> [Cond] -> IO [a]
mcmc prog cds = do
  g <- MWC.createSystemRandom
  (v, d, llTotal, _, _) <- initialStep prog cds g
  transition prog cds v d llTotal g

uncnd = unconditioned
cnd = conditioned

