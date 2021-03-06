{-# LANGUAGE TypeSynonymInstances, TemplateHaskell, QuasiQuotes, 
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             DeriveDataTypeable, ScopedTypeVariables #-} 
module Examples.UnitTests where
import System.IO.Unsafe (unsafePerformIO)
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Base
import Examples.Base
import Examples.Greedy
import Control.Monad.State
import Test.HUnit hiding (test)
-- TODO: setup modules such that they export other modules required to use them
--       (e.g. Control.Monad.State in Lib)

import Examples.BaseQuote

-----------------------------------------------------------------------------------------------------
-- Regression tests:
test = runTestTT tests
tests = TestList
  [ TestLabel "dMH"         dmh0_test
  , TestLabel "dMH"         dmh1_test
  , TestLabel "dMH"         dmh2_test
  , TestLabel "Cellar"      cellar0_test
  , TestLabel "Chancellor"  chanc0_test
  ]
mkTest   s e r = TestCase $ assertEqual s e r
mkTestIO s e r = TestCase $ assertEqual s e (unsafePerformIO r)

-----------------------------------------------------------------------------------------------------
-- Macros
dBG = defaultBaseGame
dMH = defaultMoneyHeuristic
bCE = baseCardEffects

-- Gets a tuple of important information for player #1:
-- (HAND, DECK, DISCARDPILE, ACTIONS, BUYS, MONEY)
p1Info g = ( cards.hand.p1 $ g, cards.deck.p1 $ g, cards.discardPile.p1 $ g
           , numActions.p1 $ g, numBuys.p1 $ g, amtMoney.p1 $ g )

-----------------------------------------------------------------------------------------------------
-- Tests:
dmh0_result  = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER,COPPER]} } }
dmh0_expects = Just SILVER
dmh0_test    = mkTestIO "dMH" dmh0_expects dmh0_result

dmh1_result  = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[ESTATE,SILVER]} } }
dmh1_expects = Just SILVER
dmh1_test    = mkTestIO "dMH" dmh1_expects dmh1_result

dmh2_result  = dMH $ dBG { p1 = (p1 dBG) { hand = ((hand.p1) dBG) {cards=[COPPER]} } }
dmh2_expects = Just COPPER
dmh2_test    = mkTestIO "dMH" dmh2_expects dmh2_result

cellar0_result  = evalStateT (draw 8 >> bCE CELLAR >> get >>= \g -> return $ p1Info g) greedyGame
cellar0_expects = ( [ESTATE,COPPER,COPPER,COPPER,COPPER,COPPER,COPPER,COPPER], [ESTATE], [ESTATE], 2, 1, 0 )
cellar0_test    = mkTestIO "Cellar" cellar0_expects cellar0_result

chanc0_result  = evalStateT (draw 5 >> bCE CHANCELLOR >> get >>= \g -> return $ p1Info g) greedyGame
chanc0_expects = ([COPPER,COPPER,COPPER,COPPER,COPPER],[],[COPPER,COPPER,ESTATE,ESTATE,ESTATE],1,1,2)
chanc0_test    = mkTestIO "Chancellor" chanc0_expects chanc0_result

