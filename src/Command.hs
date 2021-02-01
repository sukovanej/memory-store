{-# LANGUAGE OverloadedStrings #-}

module Command (Command (..)
               , handleCommand
               ) where

import           Bytes                   (longitudalRedudancyCheck,
                                          numberTo4Bytes, numberTo8Bytes)
import           Data                    (ResourceIndex, ResourceIndexMap,
                                          ResourceList, ResourceName,
                                          ResourceValue, SharedDatabase,
                                          getResource, getResourceList,
                                          patchResource)

import           Control.Concurrent.MVar
import qualified Data.ByteString         as B
import           Data.ByteString.Char8   (pack)
import           Data.List

data Command = CommandGet ResourceName ResourceIndex
             | CommandPatch ResourceName ResourceList ResourceIndexMap
             | CommandList ResourceName
             | CommandPing
             | CommandUnknown
             deriving (Show)


-- TODO
-- showResourceList :: ResourceList -> ByteString
-- showResourceList resourceList = intercalate (pack "--") resouceList
--
--
-- List Response
--
-- ------------------
-- | 1 byte |   -   |
-- ------------------
-- | status | items |
-- ------------------
--
-- List statuses
--  - 1 - OK
--  - 2 - RESOURCE DOESN'T EXIST
--
-- List Item
--
-- -----------------------
-- | 4 byte | SIZE bytes |
-- -----------------------
-- |  SIZE  |    items   |
-- -----------------------


handleCommand :: SharedDatabase -> Command -> IO B.ByteString

-- PING
handleCommand _ CommandPing = return "PONG"

-- UNKNOWN
handleCommand _ CommandUnknown = return "UNKNOWN"

-- GET
handleCommand sharedDb (CommandGet resourceName resourceIndex) = do
    db <- readMVar sharedDb
    let resource = getResource db resourceName resourceIndex
    return $ returnOrError resource

-- LIST
handleCommand sharedDb (CommandList resourceName) = do
    putStrLn "List started"
    db <- readMVar sharedDb
    putStrLn "db acquired"
    let resourceList = getResourceList db resourceName
    putStrLn $ "got resourceList of length " ++ show (length resourceList)
    createResourceListResponse resourceList

-- PATCH
handleCommand sharedDb (CommandPatch resourceName resourceList resourceIndexMap) = do
    patchResource resourceName resourceList resourceIndexMap sharedDb
    return successResponse


createResourceListResponse :: Maybe ResourceList -> IO B.ByteString
createResourceListResponse Nothing = return "\2"
createResourceListResponse (Just list) = do
    putStrLn $ "got " ++ show (length list) ++ " items"
    let d = B.concat $ "\1" : map createResourceListItemResponse list
    putStrLn "prepared"
    return d


createResourceListItemResponse :: ResourceValue -> B.ByteString
createResourceListItemResponse v = size <> v
    where size = numberTo4Bytes $ fromIntegral $ B.length v


returnOrError :: Maybe ResourceValue -> B.ByteString
returnOrError (Just a) = a
returnOrError Nothing  = "NOTFOUND"


successResponse :: B.ByteString
successResponse = "OK"
