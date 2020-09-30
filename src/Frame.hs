module Frame
  ( FrameTable (..)
  , Frame (..)
  , getFreeRamFrameOffset
  , setRamFrameFree
  , getFreeSwapFrameOffset
  , setSwapFrameFree
  ) where

import Foundation
import Control.Effect.State
import Types

data Frame a = Frame
             { frameId :: FrameId -- Actually, FrameId is also Frame offset
             , mem :: [Word8]
             } deriving (Show, Eq)

data FrameTable = FT { ramFreeOffs :: [Offset (Frame RAM)]
                     , ramF :: [Frame RAM] -- RAM frames
                     , swapFreeOffs :: [Offset (Frame SWAP)]
                     , swapF :: [Frame SWAP] -- SWAP frames
                     }

-- If exists, return free ram page offset
getFreeRamFrameOffset :: Has (State FrameTable) sig m => m (Maybe (Offset (Frame RAM)))
getFreeRamFrameOffset = do
  (FT rfo ram sfo swap) <- get
  case rfo of
    (x:xs) -> do
      put (FT xs ram sfo swap)
      return (Just x)
    [] -> return Nothing

setRamFrameFree :: Has (State FrameTable) sig m => Offset (Frame RAM) -> m ()
setRamFrameFree offset = do
  modify (\(FT xs ram ys sw) -> FT (offset:xs) ram ys sw)

-- If exists, return free swap page offset
getFreeSwapFrameOffset :: Has (State FrameTable) sig m => m (Maybe (Offset (Frame SWAP)))
getFreeSwapFrameOffset = do
  (FT rfo ram sfo swap) <- get
  case sfo of
    (x:xs) -> do
      put (FT rfo ram xs swap)
      return (Just x)
    [] -> return Nothing

setSwapFrameFree :: Has (State FrameTable) sig m => Offset (Frame SWAP) -> m ()
setSwapFrameFree offset = do
  modify (\(FT xs ram ys sw) -> FT xs ram (offset:ys) sw)
