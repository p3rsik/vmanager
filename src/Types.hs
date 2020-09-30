{-# LANGUAGE EmptyDataDecls #-}

module Types 
  ( RAM
  , SWAP
  , ProcessId (..)
  , Age (..)
  , FrameId (..)
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