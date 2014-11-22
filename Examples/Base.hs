{-# LANGUAGE DeriveDataTypeable,RankNTypes,FlexibleInstances,
             FlexibleContexts,KindSignatures #-}

module Examples.Base where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Engine
import Game.DeckBuild.Dominion.Lib
import Game.DeckBuild.Dominion.Base (baseCardEffects)
import Control.Monad.State

import Language.DeckBuild.Syntax
import Examples.BaseQuote

kcs = kingdomCards

-- Recommended initial game setup:
nksupply_init =
  zip
    ( map (getCard kcs)
      [COPPER, SILVER, GOLD, ESTATE, DUCHY, PROVINCE]
    ) [60    , 40    , 30  , 8     , 8    , 8       ]

kcards_init = map (getCard kcs)
  [ CELLAR, MARKET, MILITIA, MINE
  , MOAT, REMODEL, SMITHY, VILLAGE
  , WOODCUTTER, WORKSHOP ]

supply_init = (map (\c -> (c,10)) kcards_init) ++ nksupply_init

-- The default game with the default set of cards described in the Dominion rulebook:
defaultBaseGame = defaultGame
  { supply = defaultSupply { piles = supply_init }
  , doCardEffects = baseCardEffects
  , endCndn = baseEndCndn
  }

-- Whether or not the game is over for the given supply (n == # supply piles found empty already):
bEC 0 ((PROVINCE,0):_) = True           -- Province statck empty - game over
bEC 0 []               = False          -- No stacks empty - game not over
bEC 1 []               = False          -- One (non-PROVINCE) stack empty - game not over
bEC 2 []               = False          -- Two (non-PROVINCE) stacks empty - game not over
bEC 3 _                = True           -- Three stacks empty - game over
bEC n ((c,0):cs)       = bEC (n + 1) cs -- First stack empty - recurse on (n+1)
bEC n ((c,_):cs)       = bEC n cs       -- First stack NOT empty - recurse on n
baseEndCndn :: [(Card,Int)] -> Bool
baseEndCndn = bEC' 0

--test0 :: forall (m :: * -> *). (MonadState Game m, MonadIO m) => m Game
{-
test0 :: MonadIO m => m Game
test0 = execStateT (do {shuffleDrawSwap; shuffleDrawSwap }) defaultBaseGame

test1 :: MonadIO m => m Bool
test1 = evalStateT gameOver defaultBaseGame

test2 :: MonadIO m => m Game
test2 = execStateT runGame defaultBaseGame
-}

