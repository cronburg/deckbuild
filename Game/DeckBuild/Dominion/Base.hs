{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Base (baseCardEffects) where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Lib
import Control.Monad.State
import Data.Char (toUpper)
import Language.DeckBuild.Syntax hiding (Card, cID, cType, cDescr, cCost)

import Examples.BaseQuote

-- Discards any number of cards, returning the number of cards discarded
cellarEffect' :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => RuntimeCard -> m Int
cellarEffect' cellar = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g cellar
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then discard c >> cellarEffect' cellar >>= \n -> return $ n + 1
               else return 0
    Nothing -> return 0

-- Discard any number of cards, then draw that many cards:
cellarEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => RuntimeCard -> m ()
cellarEffect cellar = addActions 1 >> cellarEffect' cellar >>= \n -> draw n

-- Trash up to 4 cards
-- n == # of cards trashed so far
chapelEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => RuntimeCard -> Int -> m Int
chapelEffect _      4 = return 0
chapelEffect chapel n = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g chapel
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then trashCard c >> chapelEffect chapel (n + 1) >>= \n' -> return $ n' + 1
               else return 0
    Nothing -> return 0

-- +2 money, may put deck into discard pile
chancellorEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => RuntimeCard -> m ()
chancellorEffect chancellor = do
  addMoney 2
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g chancellor
  case c' of
    Just _  ->
      put $ g { p1 = (p1 g)
        { deck        = (deck.p1        $ g) { cards=[] }
        , discardPile = (discardPile.p1 $ g) { cards=(cards.deck.p1 $ g) ++ (cards.discardPile.p1 $ g) } } }
    Nothing -> return ()

workshopEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
workshopEffect = undefined

bureaucratEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
bureaucratEffect = undefined 

feastEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
feastEffect = undefined
 
militiaEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
militiaEffect = undefined 

moneylenderEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
moneylenderEffect = undefined 

remodelEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
remodelEffect = undefined 

smithyEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
smithyEffect = undefined
 
spyEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
spyEffect = undefined 

thiefEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
thiefEffect = undefined

throneRoomEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
throneRoomEffect = undefined 

councilRoomEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
councilRoomEffect = undefined
 
laboratoryEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
laboratoryEffect = undefined 

libraryEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
libraryEffect = undefined
 
marketEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
marketEffect = undefined
 
mineEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
mineEffect = undefined
 
witchEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
witchEffect = undefined
 
adventurerEffect :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => m ()
adventurerEffect = undefined 

baseCardEffects :: forall (m :: * -> *). (MonadIO m, MonadState Game m) => RuntimeCard -> m ()
baseCardEffects c = do
 case cID c of
  CELLAR     -> cellarEffect c
  CHAPEL     -> chapelEffect c 0 >> return ()
  MOAT       -> draw 2
  CHANCELLOR -> chancellorEffect c -- TODO: may discard
  VILLAGE    -> draw 1 >> addActions 2
  WOODCUTTER -> addBuys 1 >> addMoney 2
  WORKSHOP   -> nop -- TODO: ask gain card costing 4
  BUREAUCRAT -> gain SILVER -- TODO: rest of action
  FEAST      -> trashCard c -- TODO: gain card costing up to $5
  GARDENS    -> nop
  MILITIA    -> addMoney 2 -- TODO: other player chooses cards to discard
  MONEYLENDER -> nop -- TODO
  REMODEL    -> nop -- TODO
  SMITHY     -> draw 3
  SPY        -> draw 1 >> addActions 1 -- TODO
  THIEF      -> nop -- TODO
  THRONEROOM -> nop -- TODO
  COUNCILROOM -> draw 4 >> addBuys 1 >> swapPlayers >> draw 4 >> swapPlayers
  FESTIVAL   -> addActions 2 >> addBuys 1 >> addMoney 2
  LABORATORY -> draw 2 >> addActions 1
  LIBRARY    -> nop -- TODO
  MARKET     -> draw 1 >> addActions 1 >> addBuys 1 >> addMoney 1
  MINE       -> nop -- TODO
  WITCH      -> draw 2 >> swapPlayers >> gain CURSE >> swapPlayers
  ADVENTURER -> nop -- TODO

