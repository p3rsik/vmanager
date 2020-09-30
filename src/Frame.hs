{-# LANGUAGE TypeApplications #-}

module Frame
  ( FrameTable (..)
  , Frame (..)
  , getFreeRamFrameOffset
  , setRamFrameFree
  , getFreeSwapFrameOffset
  , setSwapFrameFree
  , writeRamFrame
  , writeSwapFrame
  , readFrame
  ) where

import Foundation
import Foundation.Collection
import Control.Effect.State
import Types

data Frame a = Frame
             { frameId :: FrameId -- Actually, FrameId is also Frame offset
             , mem :: [Word8]
             } deriving (Show, Eq)

data FrameTable = FT { ramFreeOffs :: [Offset (Frame RAM)]
                     , ramF :: NonEmpty [Frame RAM] -- RAM frames
                     , swapFreeOffs :: [Offset (Frame SWAP)]
                     , swapF :: NonEmpty [Frame SWAP] -- SWAP frames
                     }

-- If exists, return free ram page offset
getFreeRamFrameOffset :: Has (State FrameTable) sig m => m (Maybe (Offset (Frame RAM)))
getFreeRamFrameOffset = do
  (FT rfo ram sfo swap') <- get
  case rfo of
    (x:xs) -> do
      put (FT xs ram sfo swap')
      return (Just x)
    [] -> return Nothing

setRamFrameFree :: Has (State FrameTable) sig m => Offset (Frame RAM) -> m ()
setRamFrameFree offset = do
  modify (\(FT xs ram ys sw) -> FT (offset:xs) ram ys sw)

-- If exists, return free swap page offset
getFreeSwapFrameOffset :: Has (State FrameTable) sig m => m (Maybe (Offset (Frame SWAP)))
getFreeSwapFrameOffset = do
  (FT rfo ram sfo swap') <- get
  case sfo of
    (x:xs) -> do
      put (FT rfo ram xs swap')
      return (Just x)
    [] -> return Nothing

setSwapFrameFree :: Has (State FrameTable) sig m => Offset (Frame SWAP) -> m ()
setSwapFrameFree offset = do
  modify (\(FT xs ram ys sw) -> FT xs ram (offset:ys) sw)

writeRamFrame :: Has (State FrameTable) sig m => Frame RAM -> Offset (Frame RAM) -> [Word8] -> m ()
writeRamFrame f@(Frame i mem') (Offset off) tw = do
  let (s, _) = splitAt (toCount off) mem'
  let (_, e) = splitAt (toCount off + length tw) mem'
  let nf = Frame i (s <> tw <> e)
  let replace = nonEmptyFmap (\f' -> if f == f' then nf else f')
  modify $ \(FT rfo ram sfo swap') -> FT rfo (replace ram) sfo swap'

writeSwapFrame :: Has (State FrameTable) sig m => Frame SWAP -> Offset (Frame SWAP) -> [Word8] -> m ()
writeSwapFrame f@(Frame i mem') (Offset off) tw = do
  let (s, _) = splitAt (toCount off) mem'
  let (_, e) = splitAt (toCount off + length tw) mem'
  let nf = Frame i (s <> tw <> e)
  let replace = nonEmptyFmap (\f' -> if f == f' then nf else f')
  modify $ \(FT rfo ram sfo swap') -> FT rfo ram sfo $ replace swap'

readFrame :: Has (State FrameTable) sig m => Frame a -> Offset (Frame a) -> CountOf Word8 -> m ([Word8])
readFrame (Frame _ mem') (Offset off) co = do
  let (_, m) = splitAt (toCount off) mem'
  let (r, _) = splitAt co m
  return (r)