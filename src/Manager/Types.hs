module Manager.Types 
  ( MemType (..)
  , ProcessId (..)
  , Age (..)
  , FrameId (..)
  ) where

import Foundation

newtype ProcessId = Pid { unPid :: Int } deriving (Show, Eq, Ord)
newtype Age = Age { unAge :: Word } deriving (Show, Eq, Ord)
-- FrameId is also a Frame offset
newtype FrameId (a :: MemType) = Fid { unFid :: Int } deriving (Show, Eq, Ord)

data MemType = Ram | Swap deriving (Show, Eq, Ord)