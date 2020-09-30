{-# LANGUAGE EmptyDataDecls #-}

module Page
  ( PageTable
  , ProcessId (..)
  , Age (..)
  , FrameId (..)
  , Page (..)
  , RAM
  , SWAP
  , idToOff
  , offToId
  ) where 

import Foundation

newtype ProcessId = Pid { unPid :: Int } deriving (Show, Eq, Ord)
newtype Age = Age { unAge :: Word } deriving (Show, Eq, Ord)
-- FrameId is also a Frame offset
newtype FrameId = Fid { unFid :: Word } deriving (Show, Eq, Ord)

idToOff :: FrameId -> Offset a
idToOff = Offset . fromIntegral . unFid

offToId :: Offset a -> FrameId
offToId (Offset i) = Fid $ fromIntegral i
             
data RAM
data SWAP

data Page a = Page
            { frId :: FrameId
            , age :: Age -- internal age of page, used in alg
            , wBit :: Bool -- If some process is writing this page now, then no other process can write it
            , rBit :: Bool -- -//- (writing -> reading)
            , pId :: ProcessId
            } deriving (Eq, Show)

type PageTable = ([Page RAM], [Page SWAP])