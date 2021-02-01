{-# LANGUAGE OverloadedStrings #-}

module TCPServer ( startServer
                 , TCPServerHandler
                 ) where

import           Bytes                     (byteStringToWord64, debugBytesLn,
                                            longitudalRedudancyCheck,
                                            numberTo8Bytes)
import           Data                      (SharedDatabase)

import           Control.Concurrent        (forkFinally)
import qualified Control.Exception         as E
import           Control.Monad             (forever, replicateM, unless, void,
                                            when)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BC
import           Data.Maybe
import           Network.Socket
import           Network.Socket.ByteString (recv, sendAll)

type TCPServerHandler = B.ByteString -> SharedDatabase -> IO B.ByteString

data ReceiveMessageError = NOTHING_RECEIVED | SIZE_MISMATCH | TOO_BIG

type ReceivedMessageSize = B.ByteString
type ReceivedMessageData = B.ByteString
type ReceivedMessage = Either (ReceivedMessageSize, ReceivedMessageData) ReceiveMessageError

startServer :: TCPServerHandler -> SharedDatabase -> IO ()
startServer handle sharedDb = runTCPServer Nothing "3000" (talk handle sharedDb)


talk :: TCPServerHandler -> SharedDatabase -> Socket -> IO ()
talk handler sharedDb socket = do
  receivedMessage <- receiveMessage socket
  handleReceivedMessage receivedMessage socket handler sharedDb
  talk handler sharedDb socket


handleReceivedMessage :: Maybe ReceivedMessage -> Socket -> TCPServerHandler -> SharedDatabase -> IO ()
handleReceivedMessage Nothing _ _ _ = undefined
handleReceivedMessage (Just receivedMessage) socket handler sharedDb = do
  contentResponse <- handleReceivedMessageAndRespond receivedMessage handler sharedDb
  response <- createResponse contentResponse
  putStrLn $ "sending " ++ show (B.length response) ++ " bytes"
  sendAll socket response
  putStrLn "\n"


handleReceivedMessageAndRespond :: ReceivedMessage -> TCPServerHandler -> SharedDatabase -> IO B.ByteString
handleReceivedMessageAndRespond (Right TOO_BIG) _ _ = return "TOO_BIG"
handleReceivedMessageAndRespond (Right NOTHING_RECEIVED) _ _ = return "NOTHING_RECEIVED"
handleReceivedMessageAndRespond (Right SIZE_MISMATCH) _ _ = return "SIZE_MISMATCH"
handleReceivedMessageAndRespond (Left (sizeBytes, messageBytes)) handler sharedDb = do
  let content = B.init messageBytes
  let theirChecksum = B.last messageBytes
  let ourChecksum = longitudalRedudancyCheck $ sizeBytes <> content

  if B.head ourChecksum /= theirChecksum
     then do
       putStrLn "CHECKSUMERROR"
       return "CHECKSUMERROR"
     else do
       handler content sharedDb


receiveMessage :: Socket -> IO (Maybe ReceivedMessage)
receiveMessage socket = do
  -- receive length of the message
  sizeBytes <- recv socket 8

  if B.null sizeBytes
     then do
       putStrLn "NOTHING_RECEIVED"
       return Nothing
     else do
       -- receive rest of the message
       let messageSize = byteStringToWord64 sizeBytes
       if messageSize > 10 ^ 9
          then do
            putStrLn "TOO_BIG"
            print messageSize
            return $ Just $ Right TOO_BIG
          else do
            putStrLn $ show messageSize ++ " expected incomming bytes"
            messageBytes <- receiveAllData socket (fromIntegral messageSize) 8

            putStrLn $ "Received " ++ show (B.length messageBytes) ++ " bytes"

            if B.length messageBytes + 8 /= fromIntegral messageSize
               then return $ Just $ Right SIZE_MISMATCH
               else return $ Just $ Left (sizeBytes, messageBytes)



receiveAllData :: Socket -> Int -> Int -> IO B.ByteString
receiveAllData socket expectedSize currentSize = do
  let chunkSize = 3000
  let remainingSize = expectedSize - currentSize
  response <- recv socket chunkSize
  let responseLength = B.length response

  if remainingSize == responseLength
     then return response
     else do
       nextResponse <- receiveAllData socket expectedSize (currentSize + responseLength)
       return $ response <> nextResponse


-- from the "network-run" package.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        putStrLn $ "Listenning on " ++ show port
        return sock
    loop sock = forever $ do
        (conn, _peer) <- accept sock
        void $ forkFinally (server conn) (const $ gracefulClose conn 5000)


-- calculate size and checksum
createResponse :: B.ByteString -> IO B.ByteString
createResponse v = do
    let size = numberTo8Bytes $ fromIntegral $ B.length v + 1 + 8  -- +1 for the checksum byte and +8 for the size bytes
    let dataWithoutCheckSum = size <> v
    let checkSum = longitudalRedudancyCheck dataWithoutCheckSum
    let response = dataWithoutCheckSum <> checkSum
    return response
