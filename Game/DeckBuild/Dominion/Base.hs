{-# LANGUAGE DeriveDataTypeable, RankNTypes, FlexibleInstances, FlexibleContexts,
             KindSignatures, ScopedTypeVariables #-}
module Game.DeckBuild.Dominion.Base (baseCardEffects) where
import Game.DeckBuild.Dominion.Types
import Game.DeckBuild.Dominion.Lib
import Control.Monad.State
import Data.Char (toUpper)
import Language.DeckBuild.Syntax hiding (Card, cID, cType, cDescr, cCost)

import Examples.BaseQuote
import Game.Sample.Hakaru

-- Discards any number of cards, returning the number of cards discarded
cellarEffect' :: Measure Int 
cellarEffect' = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g CELLAR
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then discard c >> cellarEffect' >>= \n -> return $ n + 1
               else return 0
    Nothing -> return 0

-- Discard any number of cards, then draw that many cards:
cellarEffect :: Measure () 
cellarEffect = addActions 1 >> cellarEffect' >>= \n -> draw n

-- Trash up to 4 cards
-- n == # of cards trashed so far
chapelEffect :: Int -> Measure Int 
chapelEffect 4 = return 0
chapelEffect n = do
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g CHAPEL
  case c' of
    Just c  -> if   elem c ((cards.hand.p1) g)
               then trashCard c >> chapelEffect (n + 1) >>= \n' -> return $ n' + 1
               else return 0
    Nothing -> return 0

-- +2 money, may put deck into discard pile
chancellorEffect :: Measure () 
chancellorEffect = do
  addMoney 2
  g  <- get
  c' <- liftIO $ ((mayPick.p1) g) g CHANCELLOR
  case c' of
    Just _  ->
      put $ g { p1 = (p1 g)
        { deck        = (deck.p1        $ g) { cards=[] }
        , discardPile = (discardPile.p1 $ g) { cards=(cards.deck.p1 $ g) ++ (cards.discardPile.p1 $ g) } } }
    Nothing -> return ()

workshopEffect :: Measure () 
workshopEffect = undefined

bureaucratEffect :: Measure () 
bureaucratEffect = undefined 

feastEffect :: Measure () 
feastEffect = undefined
 
militiaEffect :: Measure () 
militiaEffect = undefined 

moneylenderEffect :: Measure () 
moneylenderEffect = undefined 

remodelEffect :: Measure () 
remodelEffect = undefined 

smithyEffect :: Measure () 
smithyEffect = undefined
 
spyEffect :: Measure () 
spyEffect = undefined 

thiefEffect :: Measure () 
thiefEffect = undefined

throneRoomEffect :: Measure () 
throneRoomEffect = undefined 

councilRoomEffect :: Measure () 
councilRoomEffect = undefined
 
laboratoryEffect :: Measure () 
laboratoryEffect = undefined 

libraryEffect :: Measure () 
libraryEffect = undefined
 
marketEffect :: Measure () 
marketEffect = undefined
 
mineEffect :: Measure () 
mineEffect = undefined
 
witchEffect :: Measure () 
witchEffect = undefined
 
adventurerEffect :: Measure () 
adventurerEffect = undefined 

baseCardEffects :: CardName -> Measure ()
baseCardEffects c = case c of
  CELLAR     -> cellarEffect
  CHAPEL     -> chapelEffect 0 >> return ()
  MOAT       -> draw 2
  CHANCELLOR -> chancellorEffect -- TODO: may discard
  VILLAGE    -> draw 1 >> addActions 2
  WOODCUTTER -> addBuys 1 >> addMoney 2
  WORKSHOP   -> nop -- TODO: ask gain card costing 4
  BUREAUCRAT -> gain SILVER -- TODO: rest of action
  FEAST      -> trashCard FEAST -- TODO: gain card costing up to $5
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
  otherwise  -> nop

