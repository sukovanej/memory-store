{-# LANGUAGE OverloadedStrings #-}

module CommandHandler ( handleMessage
                      ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map              as M

import           Bytes                 (byteStringToWord32)
import           Command               (Command (..), handleCommand)
import           Data                  (ResourceData (ResourceData),
                                        ResourceIndex, ResourceIndexMap,
                                        ResourceList, ResourceValue)
import           TCPServer             (TCPServerHandler)


-- == CONTENT ========================================================
--
-- -----------------------------------------------------------
-- | 8 bytes  | 1 bytes  | SIZE - 8 - 1 - 4 bytes | 4 bytes  |
-- -----------------------------------------------------------
-- |   SIZE   |  method  |           content      | checksum |
-- -----------------------------------------------------------
--
-- == BODY ===========================================================
--
-- PING  '\1'
--
-- GET   '\2'
--
-- --------------------------------------------------
-- | 4 bytes   | NAME-SIZE bytes | INDEX-SIZE bytes |
-- --------------------------------------------------
-- | NAME-SIZE |  resource name  |  resource index  |
-- --------------------------------------------------
--
-- LIST  '\3'
--
-- -------------------
-- |        -        |
-- -------------------
-- |  resource name  |
-- -------------------
--
-- PATCH '\4'
--
-- -----------------------------------------
-- | 4 bytes   | NAME-SIZE bytes | items[] |
-- -----------------------------------------
-- | NAME-SIZE | resource name   |         |
-- -----------------------------------------
--
--     ITEM
--
--     -------------------------------------------------------------
--     |  4 bytes  | NAME-SIZE bytes |  4 bytes  | ITEM-SIZE bytes |
--     -------------------------------------------------------------
--     | NAME-SIZE |    index name   | ITEM-SIZE |      content    |
--     -------------------------------------------------------------

-- Main handler
handleMessage :: TCPServerHandler
handleMessage message sharedDb = do
  let methodCode = BC.head message
  let content = BC.tail message
  let command = getCommandByMethodCode methodCode content
  handleCommand sharedDb command


-- Resolve method code
getCommandByMethodCode :: Char -> B.ByteString -> Command
getCommandByMethodCode '\1' = handlePingMessage
getCommandByMethodCode '\2' = handleGetMessage
getCommandByMethodCode '\3' = handleListMessage
getCommandByMethodCode '\4' = handlePatchMessage
getCommandByMethodCode _    = const CommandUnknown


-- PING
handlePingMessage :: B.ByteString -> Command
handlePingMessage _ = CommandPing


-- GET
handleGetMessage :: B.ByteString -> Command
handleGetMessage message = CommandGet resourceName resourceIndex
    where resourceNameSize = fromIntegral $ byteStringToWord32 message
          resourceName = B.take resourceNameSize . B.drop 4 $ message
          resourceIndex = B.drop (resourceNameSize + 4) message


-- LIST
handleListMessage :: B.ByteString -> Command
handleListMessage = CommandList


-- PATCH
--
-- TODO: idea
-- Use state monad for message parsing?
handlePatchMessage :: B.ByteString -> Command
handlePatchMessage message = CommandPatch resourceName resourceList resourceIndexMap
  where  (resourceName, remainingMessage) = parsePatchMessageResourceName message
         (resourceList, resourceIndexMap) = parsePatchMessageItems remainingMessage [] M.empty 0


parsePatchMessageItems :: B.ByteString -> ResourceList -> ResourceIndexMap -> Int -> (ResourceList, ResourceIndexMap)
parsePatchMessageItems msg list indexMap counter =
  if B.length msg > 0
     then let (resourceIndex, resourceValue, remainingMessage) = parsePatchMessageItem msg
           in parsePatchMessageItems remainingMessage (list ++ [resourceValue]) (M.insert resourceIndex counter indexMap) (counter + 1)
     else (list, indexMap)


parsePatchMessageResourceName :: B.ByteString -> (B.ByteString, B.ByteString)
parsePatchMessageResourceName message = (resourceName, remainingMessage)
    where resourceNameSize = fromIntegral $ byteStringToWord32 message
          resourceName = B.take resourceNameSize . B.drop 4 $ message
          remainingMessage = B.drop (resourceNameSize + 4) message


parsePatchMessageItem :: B.ByteString -> (ResourceIndex, ResourceValue, B.ByteString)
parsePatchMessageItem message = (resourceIndex, resourceName, remainingMessage)
    where resourceIndexSize = fromIntegral $ byteStringToWord32 message -- parse index
          resourceIndex = B.take resourceIndexSize . B.drop 4 $ message
          afterIndexMessage = B.drop (resourceIndexSize + 4) message

          resourceNameSize = fromIntegral $ byteStringToWord32 afterIndexMessage -- parse name
          resourceName = B.take resourceNameSize . B.drop  4 $ afterIndexMessage
          remainingMessage = B.drop (resourceNameSize + 4) afterIndexMessage
