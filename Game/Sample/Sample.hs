{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.Sample.Sample where
-- Useful sampling functions used when playing games
--import Language.Hakaru.Metropolis hiding (sample)
import Language.Hakaru.Types -- Discrete
import Language.Hakaru.Distribution

import System.Random.Shuffle (shuffle,shuffle')
import System.Random (newStdGen)
import Data.Typeable (Typeable)

import Game.Sample.Hakaru

fI = fromIntegral

-- Importance Sampler macros:
sample1 :: (Typeable a, Show a, Ord a) => Measure a -> [Cond] -> IO a
sample1 fncn conds = do
    s <- sampleN 1 fncn conds
    return $ head s

sampleN :: (Typeable a, Show a, Ord a) => Int -> Measure a -> [Cond] -> IO [a]
sampleN n fncn conds = do
    t <- sample fncn conds
    return $ take n $ map fst t

--uncnd :: Typeable a => Dist a -> Measure a
--uncnd = unconditioned
--cnd :: Typeable a => Dist a -> Measure a
--cnd  = conditioned

-- Get a uniform int on a mn-closed mx-open interval [mn,mx)
uniformInt :: Int -> Int -> Measure Int
uniformInt mn' mx' = do
    let (mn,mx) = (fI mn', fI mx') :: (Double,Double)
    dbl <- (uncnd $ uniform mn mx) :: Measure Double
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

shuffleList :: (Typeable a, Eq a) => [a] -> Measure [a]
shuffleList []     = uncnd $ categorical [([],1)]
shuffleList (x:[]) = uncnd $ categorical [([x],1)]
shuffleList xs     = do
  us <- sequence [uniformInt 0 ((length xs) - i + 1) | i <- [1..(length xs)-1]]
  return $ shuffle xs us
--shuffleList (x:xs) = do
--  p   <- uniformInt 0 $ (length xs) + 1 -- partition
--  lhs <- shuffleList (take p xs) -- shuffle left of partition
--  rhs <- shuffleList (drop p xs) -- shuffle right of partition
--  return $ lhs ++ [x] ++ rhs     -- merge partitions

