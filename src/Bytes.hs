{-# LANGUAGE OverloadedStrings #-}

module Bytes ( byteStringToWord64
             , byteStringToWord32
             , debugBytesLn
             , numberTo4Bytes
             , numberTo8Bytes
             , longitudalRedudancyCheck
             ) where

import           Data.Bits
import qualified Data.ByteString as B
import           Data.List
import           Data.Word


byteStringToWord64 :: B.ByteString -> Word64
byteStringToWord64 v = B.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 $ B.take 8 v


byteStringToWord32 :: B.ByteString -> Word32
byteStringToWord32 v = B.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 $ B.take 4 v


numberTo4Bytes :: Word32 -> B.ByteString
numberTo4Bytes v = B.pack $ map fromIntegral [ (v .&. 0XFF000000) `shiftR` 24
                                             , (v .&. 0XFF0000) `shiftR` 16
                                             , (v .&. 0XFF00) `shiftR` 8
                                             , v .&. 0XFF
                                             ]

numberTo8Bytes :: Word64 -> B.ByteString
numberTo8Bytes v = B.pack $ map fromIntegral [ (v .&. 0XFF00000000000000) `shiftR` 56
                                             , (v .&. 0XFF000000000000) `shiftR` 48
                                             , (v .&. 0XFF0000000000) `shiftR` 40
                                             , (v .&. 0XFF00000000) `shiftR` 32
                                             , (v .&. 0XFF000000) `shiftR` 24
                                             , (v .&. 0XFF0000) `shiftR` 16
                                             , (v .&. 0XFF00) `shiftR` 8
                                             , v .&. 0XFF
                                             ]


debugBytesLn :: B.ByteString -> IO ()
debugBytesLn = putStrLn . unwords . map show . B.unpack


-- LRC check sum
-- https://en.wikipedia.org/wiki/Longitudinal_redundancy_check
longitudalRedudancyCheck :: B.ByteString -> B.ByteString
longitudalRedudancyCheck v = B.pack [calc 0 (B.unpack v)]
    where calc :: Word8 -> [Word8] -> Word8
          calc a b = if null b then ((fromIntegral a `xor` 0xFF) + 1) .&. 0xFF
                               else calc ((a + fromIntegral (head b)) .&. 0xFF) (tail b)
