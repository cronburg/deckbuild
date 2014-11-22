{-# LANGUAGE OverloadedStrings #-}
module API.WebServer where
  -- TODO: Move this into an "AI" module under "Dominion"

  import Game.DeckBuild.Dominion.Lib
  import Game.DeckBuild.Dominion.Engine
  import Game.DeckBuild.Dominion.Types
  import Game.Sample.Sample
  import Haskell.Macros
  import Examples.Base
  import qualified Language.Hakaru.ImportanceSampler as IS
  import Language.Hakaru.Metropolis
  import Language.Hakaru.Types -- Discrete
  import Language.Hakaru.Distribution
  import Control.Monad.State
  import Data.List (maximumBy)
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
                where
                    client     = ((somePlayer (T.unpack msg) conn), conn)
                    disconnect = do
                          -- Remove client and return new state
                          s <- modifyMVar state $ \s ->
                              let s' = removeClient client s in return (s', s')
                          broadcast ((whois client) `mappend` " disconnected") s

  talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
  talk conn state client = forever $ do
    msg <- WS.receiveData conn
    liftIO $ readMVar state >>= broadcast
      ((whois client) `mappend` ": " `mappend` msg)

  whois :: Client -> Text
  whois client = T.pack $ name $ fst client

  {- GAME LOGIC -}
  somePlayer n conn = defaultPlayer
    { name = n
    , buyHeuristic = (myBuyHeuristic conn)
    , actHeuristic = (myActHeuristic conn)
    , mayPick      = undefined
    , mustPick     = undefined
    }

  myBuyHeuristic :: WS.Connection -> Game -> IO (Maybe Card)  
  myBuyHeuristic conn g =


  playerGame = defaultBaseGame
    { p1 = somePlayer "Greedy1"
    , p2 = somePlayer "Greedy2"
    }

  -- Run our simplistic greedy vs greedy game:
  runPlayerGame :: MonadIO m => m Game
  runPlayerGame = execStateT runGame playerGame
