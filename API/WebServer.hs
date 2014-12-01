{-# LANGUAGE OverloadedStrings #-}
module API.WebServer where

  import Game.DeckBuild.Dominion.Lib
  import Game.DeckBuild.Dominion.Engine
  import Game.DeckBuild.Dominion.Types
  import Game.Sample.Sample
  import Haskell.Macros
  import Examples.Base
  import Language.DeckBuild.Syntax hiding (cID,cType,cDescr,cCost)
  import Examples.BaseQuote
  import qualified Language.Hakaru.ImportanceSampler as IS
  import Language.Hakaru.Metropolis
  import Language.Hakaru.Types -- Discrete
  import Language.Hakaru.Distribution
  import Control.Monad.State
  import Data.List (maximumBy, elemIndex)
  import Data.Ord (comparing)
  import Data.Char (isPunctuation, isSpace)
  import Data.Monoid (mappend)
  import Data.Text (Text)
  import Control.Exception (finally)
  import Control.Monad (forM_, forever)
  import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
  import Control.Monad.IO.Class (liftIO)
  import qualified Data.Text as T
  import qualified Data.Text.IO as T

  import qualified Network.WebSockets as WS

  {- WEBSOCKET STUFF: based off of the websocket simple chat example -}

  type Client = (Player, WS.Connection)

  type ServerState = [Client]

  --Create a new, initial state:

  newServerState :: ServerState
  newServerState = []

  --Get the number of active clients:

  numClients :: ServerState -> Int
  numClients = length

  --Check if a user already exists (based on username):

  clientExists :: Client -> ServerState -> Bool
  clientExists client = any ((== fst client) . fst)

  --Add a client

  addClient :: Client -> ServerState -> ServerState
  addClient client clients = client : clients

  --Remove a client:

  removeClient :: Client -> ServerState -> ServerState
  removeClient client = filter ((/= fst client) . fst)

  --Send a message to all clients, and log it on stdout:

  broadcast :: Text -> ServerState -> IO ()
  broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

  {-
  The main function first creates a new state for the server, then spawns the
  actual server. For this purpose, we use the simple server provided by
  `WS.runServer`.
  -}


  main :: IO ()
  main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

  application :: MVar ServerState -> WS.ServerApp
  application state pending = do
    conn <- WS.acceptRequest pending
    WS.sendTextData conn ("Welcome to Deck Build" :: Text)
    WS.sendTextData conn ("Please enter your player name (no spaces):" :: Text)
    msg <- WS.receiveData conn
    clients <- liftIO $ readMVar state
    case msg of
          _   | any ($ (whois client))
                  [T.null, T.any isPunctuation, T.any isSpace] ->
                      WS.sendTextData conn ("Name cannot " `mappend`
                          "contain punctuation or whitespace, and " `mappend`
                          "cannot be empty" :: Text)
              | clientExists client clients ->
                  WS.sendTextData conn ("User already exists" :: Text)
              | length clients >= 2 ->
                  WS.sendTextData conn ("Too many players" :: Text)
              --success
              | otherwise -> flip finally disconnect $ do
                liftIO $ modifyMVar_ state $ \s -> do
                  let s' = addClient client s
                  WS.sendTextData conn $
                    "Welcome! Users: " `mappend`
                    T.intercalate ", " (map whois s)
                  broadcast ((whois client) `mappend` " joined") s'
                  return s'
                talk conn state client
                --execStateT runGame playerGame
            where
              client     = ((somePlayer (T.unpack msg) state conn), conn)
              disconnect = do
                -- Remove client and return new state
                s <- modifyMVar state $ \s ->
                  let s' = removeClient client s in return (s', s')
                broadcast ((whois client) `mappend` " disconnected") s


  talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
  talk conn state client = forever $ do
    clients <- liftIO $ readMVar state
    case numClients clients of
      2 | whois client == (whois . head) clients -> do
                execStateT runGame playerGame
                broadcast (T.pack "~~~ Game Over ~~~") clients
                return ()
        | otherwise ->
            return ()
       where
         playerGame = defaultBaseGame
          { p1 = (fst . head) clients
          , p2 = (fst . last) clients
          , maxTurns = 10
          , endPrint = (myEndPrint clients)
          }
      _ -> return ()

    --msg <- WS.receiveData conn
  myEndPrint clients g = liftIO $ do
      broadcast (T.pack $"end of game: " ++ show  g) clients
      return ()


  whois :: Client -> Text
  whois client = T.pack $ name $ fst client

  {- GAME LOGIC -}
  somePlayer n state conn = defaultPlayer
    { name = n
    , buyHeuristic = (myBuyHeuristic state conn)
    , actHeuristic = (myActHeuristic state conn)
    , mayPick      = greedyMayPick
    , mustPick     = greedyMustPick
    }

  myBuyHeuristic :: MVar ServerState -> WS.Connection -> Game -> IO (Maybe CardName)
  myBuyHeuristic state conn g = do
    --ask the user what they want to buy
    clients <- liftIO $ readMVar state
    cardsICanBuy <- return (filter (canBuy g) (map fst ((piles . supply) g)))
    strCards <- return (map show cardsICanBuy)
    WS.sendTextData conn (T.pack ("you have: " ++ show ((amtMoney . p1) g) ++ " monies and " ++ show ((numBuys . p1) g) ++ " buys, and can buy: " ++ show strCards ))
    WS.sendTextData conn (T.pack "What card would you like to buy:")
    msg <- WS.receiveData conn
    cardNToBuy <- case ( (T.unpack (T.toUpper msg)) `elemIndex` strCards) of
      Just n -> do
                   broadcast (T.pack (((name . p1) g)) `mappend` " would like to buy " `mappend` msg) clients
                   return (Just (cardsICanBuy !! n))
      Nothing -> do return Nothing
    --cardsICanBuy !! ((T.unpack msg) `elemIndex` strCards)
    --broadcast (T.pack (show g)) clients

    return cardNToBuy


  myActHeuristic :: MVar ServerState -> WS.Connection -> Game -> IO (Maybe CardName)
  myActHeuristic state conn g = do
    clients <- liftIO $ readMVar state
    as <- return (filter isAction $ (cards.hand.p1) g)
    WS.sendTextData conn (T.pack ("you can play: " ++ show as ++ "."))
    case length as of
      0 -> do
        WS.sendTextData conn (T.pack ("There are no cards for you to play"))
        return Nothing
      _ -> do
        strCards <- return (map show as)
        WS.sendTextData conn (T.pack "What card would you like to play:")
        msg <- WS.receiveData conn
        cardNToPlay <- case ( (T.unpack (T.toUpper msg)) `elemIndex` strCards) of
          Just n -> do
            broadcast (T.pack (((name . p1) g)) `mappend` " would like to play " `mappend` msg) clients
            return (Just (as !! n))
          Nothing -> do return Nothing
        return cardNToPlay


  cellarPick g = maybeHead $ filter isVictory ((cards.hand.p1) g)
  -- c' == the card which caused us to have to maybe pick a card
  greedyMayPick :: Game -> CardName -> IO (Maybe CardName)
  greedyMayPick g c' = return $ case c' of
    CELLAR     -> cellarPick g -- pick a victory card in hand if exists
    CHANCELLOR -> Just COPPER  -- any card triggers a discard deck
    otherwise  -> Nothing

  -- c' == the card which caused us to have to pick a card
  greedyMustPick :: Game -> CardName -> IO CardName
  greedyMustPick g c' = undefined

    -- ? cardname
     -- case find ((==) (cID c)) kingdomeCards of
       --Just card -> print info for card;
       -- Nothing -> print "card not found"
