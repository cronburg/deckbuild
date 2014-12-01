--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module API.CmdClient
( main
) where


  --------------------------------------------------------------------------------
  import           Control.Concurrent  (forkIO)
  import           Control.Monad       (forever, unless)
  import           Control.Monad.Trans (liftIO)
  import           Network.Socket      (withSocketsDo)
  import           Data.Text           (Text)
  import qualified Data.Text           as T
  import qualified Data.Text.IO        as T
  import qualified Network.WebSockets  as WS
  import           System.Console.Readline as RL

  --------------------------------------------------------------------------------
  app :: WS.ClientApp ()
  app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
      msg <- WS.receiveData conn
      liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do{
      line <- RL.readline ""
     ; case line of
        Nothing -> return ()
        Just s -> WS.sendTextData conn (T.pack s) >> loop
    }
    loop
    WS.sendClose conn ("Bye!" :: Text)


  --------------------------------------------------------------------------------
  main :: IO ()
  main = do
    liftIO $ T.putStrLn "Please enter the ip address of the game server or localhost:"
    addr <- RL.readline ""
    case addr of
      Nothing -> main
      Just s -> withSocketsDo $ WS.runClient s 9160 "/" app
