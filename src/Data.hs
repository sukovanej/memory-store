module Data ( getResourceList
            , getResource
            , patchResource
            , newSharedDatabase
            , SharedDatabase
            , ResourceName
            , ResourceValue
            , ResourceIndex
            , ResourceList
            , ResourceIndexMap
            , ResourceData (..)
            ) where

import           Control.Concurrent.MVar

import           Data.ByteString                (ByteString)
import           Data.Functor
import qualified Data.Map                       as Map


type ResourceName = ByteString
type ResourceValue = ByteString
type ResourceIndex = ByteString


type ResourceIndexMap = Map.Map ResourceIndex Int
type ResourceList = [ResourceValue]

data ResourceData = ResourceData { resourceList     :: ResourceList
                                 , resourceIndexMap :: ResourceIndexMap
                                 }

type Database = Map.Map ResourceName ResourceData

type SharedDatabase = MVar Database


getResourceList :: Database -> ResourceName -> Maybe ResourceList
getResourceList db resourceName = maybeResourceData <&> resourceList
    where maybeResourceData = Map.lookup resourceName db


getResource :: Database -> ResourceName -> ResourceIndex -> Maybe ResourceValue
getResource db resourceName resourceIndex = do
    resourceData <- Map.lookup resourceName db

    let list = resourceList resourceData
    let indexMap = resourceIndexMap resourceData

    resourceIndex <- Map.lookup resourceIndex indexMap
    return $ list !! resourceIndex


patchResource :: ResourceName -> ResourceList -> ResourceIndexMap -> SharedDatabase -> IO ()
patchResource resourceName resourceList resourceIndexMap sharedDb = modifyMVar_ sharedDb update
    where update = return . safePatchDatabase resourceName resourceList resourceIndexMap


safePatchDatabase :: ResourceName -> ResourceList -> ResourceIndexMap -> Database -> Database
safePatchDatabase resourceName resourceList resourceIndexMap =
    Map.insert resourceName (ResourceData resourceList resourceIndexMap)


newSharedDatabase :: IO SharedDatabase
newSharedDatabase = newMVar Map.empty
