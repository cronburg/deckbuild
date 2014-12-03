{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.Sample.Sample where
-- Useful sampling functions used when playing games
import qualified Language.Hakaru.ImportanceSampler as IS
import Language.Hakaru.Metropolis hiding (sample)
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import System.Random.Shuffle (shuffle')
import System.Random (newStdGen)
import Data.Typeable (Typeable)

import Game.Sample.Hakaru

fI = fromIntegral

-- Importance Sampler macros:
sample1 :: (Show a, Ord a) => IS.Measure a -> [Cond] -> IO a
sample1 fncn conds = do
    s <- sampleN 1 fncn conds
    return $ head s

sampleN :: (Show a, Ord a) => Int -> IS.Measure a -> [Cond] -> IO [a]
sampleN n fncn conds = do
    t <- sample fncn conds
    return $ take n $ map fst t

uncnd :: Typeable a => Dist a -> IS.Measure a
uncnd = IS.unconditioned

cnd :: Typeable a => Dist a -> IS.Measure a
cnd  = IS.conditioned

-- Get a uniform int on a mn-closed mx-open interval [mn,mx)
uniformInt :: Int -> Int -> IS.Measure Int
uniformInt mn' mx' = do
    let (mn,mx) = (fI mn', fI mx') :: (Double,Double)
    dbl <- (uncnd $ uniform mn mx) :: IS.Measure Double
    if ((==) dbl mx) then return $ truncate mx
    else return $ floor dbl

-- Shuffling using hakaru:
--shuffleC' [] d2 = return d2
--shuffleC' d1 d2 = do
--    idx <- uniformInt 0 (length d1)
--    return $ (d1 !! idx) : (removeNth idx d1)
--shuffleCards' d = sample1 (shuffleC' d []) []

-- Shuffling using System.Random:
--shuffleList :: [a] -> IO [a]
--shuffleList d = do
--    g <- newStdGen -- TODO: keep random gen in game data type
--    return $ shuffle' d (length d) g

shuffleList :: (Typeable a, Eq a) => [a] -> IS.Measure [a]
shuffleList []     = uncnd $ categorical [([],1)]
shuffleList (x:[]) = uncnd $ categorical [([x],1)]
shuffleList (x:xs) = do
  p   <- uniformInt 0 $ (length xs) + 1 -- partition
  lhs <- shuffleList (take p xs) -- shuffle left of partition
  rhs <- shuffleList (drop p xs) -- shuffle right of partition
  return $ lhs ++ [x] ++ rhs     -- merge partitions

