module Client where
import System.IO
import System.IO.Error
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import qualified Network
import qualified Parser
import Types    


connect :: Network.HostName -> Network.PortID -> IO ()
connect hostname port = do
  channel <- atomically $ newTChan
  handle <- Network.connectTo hostname port
  forkIO $ recvThread handle channel
  return ()
  

recvThread :: Handle -> TChan Message -> IO ()
recvThread handle channel = do
  maybeMsg <- tryIOError $ hGetLine handle
  case maybeMsg of
    Left e -> do
        putStrLn "They disconnected"
        hClose handle
    Right msg -> do
        case Parser.parse msg of
          Just message -> atomically $ writeTChan channel message
          Nothing -> putStrLn $ "Bad message" ++ msg
