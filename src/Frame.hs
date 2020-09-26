module Frame
  ( FrameTable (..)
  , Frame (..)
  , popFreeRamPage
  , popFreeSwapPage
  , getFreeRamPage
  , getFreeSwapPage
  ) where

import Foundation

data Frame = Frame
             { frameId :: Word
             , mem :: [Word8]
             } deriving (Show, Eq)

data FrameTable = FrameTable [Word] [Frame] [Word] [Frame]
-- [Word] - array with offsets of free Frames in RAM
-- [Frame] - RAM frames
-- [Word](snd) - array with offsets of free Frames in SWAP
-- [Frame](snd) - SWAP frames

-- pop physical page making it not-free
popFreeRamPage :: FrameTable -> FrameTable
popFreeRamPage (FrameTable (_:xs) y z zs) = FrameTable xs y z zs
popFreeRamPage (FrameTable [] y z zs) = FrameTable [] y z zs

-- get free physical page
getFreeRamPage :: FrameTable -> Maybe Word
getFreeRamPage (FrameTable (x:_) _ _ _) = Just x
getFreeRamPage (FrameTable [] _ _ _) = Nothing

-- pop physical page making it not-free
popFreeSwapPage :: FrameTable -> FrameTable
popFreeSwapPage (FrameTable xs y (_:z) zs) = FrameTable xs y z zs
popFreeSwapPage (FrameTable xs y [] zs) = FrameTable xs y [] zs

-- get free physical page
getFreeSwapPage :: FrameTable -> Maybe Word
getFreeSwapPage (FrameTable _ _ (x:_) _) = Just x
getFreeSwapPage (FrameTable _ _ [] _) = Nothing
