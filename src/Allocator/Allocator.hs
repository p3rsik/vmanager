module Allocator.Allocator where

import Allocator.Process
import Foundation
import Control.Effect.Error
import Control.Effect.State
import Data.HashMap (Map)

-- map of process id to its pages
type PMap = Map Pid Mem

data AllocError = OutOfMemory deriving (Eq, Ord)

type AllocSig sig m = ( Has (State PMap) sig m
                      , Has (Throw AllocError) sig m
                      , Has (Catch AllocError) sig m)


class Allocator a where
    alloc   :: Has (State PMap) sig m => a -> [Word8] -> m (Maybe (Word8))
    realloc :: Has (State PMap) sig m => a -> Word8 -> m (Word8)
    free    :: Has (State PMap) sig m => a -> m (Maybe (Word8))

instance Allocator Process where
    alloc = undefined
    realloc = undefined
    free = undefined