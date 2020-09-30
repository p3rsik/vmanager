module Frame
  ( FrameTable (..)
  , Frame (..)
  , Frames (..)
  ) where

import Foundation
import Foundation.Collection
import Control.Effect.State
import Types

data Frame (a :: MemType) = Frame
             { frameId :: FrameId -- Actually, FrameId is also Frame offset
             , mem :: [Word8]
             } deriving (Show, Eq)

data FrameTable = FT { ramFreeOffs :: [Offset (Frame 'Ram)]
                     , ramF :: NonEmpty [Frame 'Ram] -- RAM frames
                     , swapFreeOffs :: [Offset (Frame 'Swap)]
                     , swapF :: NonEmpty [Frame 'Swap] -- SWAP frames
                     }

class Frames a where
  getFreeFrameOffset :: Has (State FrameTable) sig m => m (Maybe (Offset (Frame a)))
  setFrameFree :: Has (State FrameTable) sig m => Offset (Frame a) -> m ()
  getFrame :: Has (State FrameTable) sig m => FrameId -> m (Frame a)
  writeFrame :: Has (State FrameTable) sig m => Frame a -> Offset Word8 -> [Word8] -> m ()
  readFrame :: Has (State FrameTable) sig m => Frame a -> Offset (Frame a) -> CountOf Word8 -> m ([Word8])

readFrame' :: Has (State FrameTable) sib m => Frame a -> Offset (Frame a) -> CountOf Word8 -> m ([Word8])
readFrame' (Frame _ mem') (Offset off) co = do
 let (_, m) = splitAt (toCount off) mem'
 let (r, _) = splitAt co m
 return (r)

instance Frames 'Ram where
  getFreeFrameOffset = do
    (FT rfo ram sfo swap') <- get
    case rfo of
      (x:xs) -> do
        put (FT xs ram sfo swap')
        return (Just x)
      [] -> return Nothing

  setFrameFree offset = do
    modify (\(FT xs ram ys sw) -> FT (offset:xs) ram ys sw)

  getFrame fId = do
    (FT _ ram _ _) <- get
    case getNonEmpty ram ! idToOff fId of
      Just f -> return f
      -- This case would never be reached (I hope)
      Nothing -> undefined

  writeFrame f@(Frame i mem') (Offset off) tw = do
    let (s, _) = splitAt (toCount off) mem'
    let (_, e) = splitAt (toCount off + length tw) mem'
    let nf = Frame i (s <> tw <> e)
    let replace = nonEmptyFmap (\f' -> if f == f' then nf else f')
    modify $ \(FT rfo ram sfo swap') -> FT rfo (replace ram) sfo swap'

  readFrame = readFrame'

instance Frames 'Swap where
  getFreeFrameOffset = do
    (FT rfo ram sfo swap') <- get
    case sfo of
      (x:xs) -> do
        put (FT rfo ram xs swap')
        return (Just x)
      [] -> return Nothing

  setFrameFree offset = do
    modify (\(FT xs ram ys sw) -> FT xs ram (offset:ys) sw)

  getFrame fId = do
    (FT _ _ _ sw) <- get
    case getNonEmpty sw ! idToOff fId of
      Just f -> return f
      -- This case would never be reached (I hope)
      Nothing -> undefined

  writeFrame f@(Frame i mem') (Offset off) tw = do
    let (s, _) = splitAt (toCount off) mem'
    let (_, e) = splitAt (toCount off + length tw) mem'
    let nf = Frame i (s <> tw <> e)
    let replace = nonEmptyFmap (\f' -> if f == f' then nf else f')
    modify $ \(FT rfo ram sfo swap') -> FT rfo ram sfo $ replace swap'

  readFrame = readFrame'