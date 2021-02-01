{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map       as Map

import           CommandHandler (handleMessage)
import           Data           (newSharedDatabase)
import           TCPServer      (startServer)

main :: IO ()
main = do
    putStrLn "Starting the server..."
    sharedDb <- newSharedDatabase
    startServer handleMessage sharedDb
