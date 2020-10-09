module Allocator.Allocator where

import Env
import Manager
import Foundation
import Control.Effect.Error
import Control.Effect.State
import Data.HashMap.Lazy
import Control.Monad

-- map of processes to its pages
type PMap = HashMap ProcessId [Page 'Ram]
type Ptr = Int
newtype Size = Size { unSize :: CountOf Word8 } deriving (Eq, Show)

data AllocError = OutOfMemory deriving (Eq, Ord)

type AllocSig sig m = ( Has (State PMap) sig m
                      , Has (Throw AllocError) sig m
                      , Has (Catch AllocError) sig m)



-- TODO mock of env, actually should be exported from somewhere
env :: Env
env = Env 1024 0 0

class Allocator a where
    alloc   :: (ManagerSig sig m, Has (State PMap) sig m) => a -> Size -> m (Maybe Ptr)
    realloc :: (ManagerSig sig m, Has (State PMap) sig m) => a -> Ptr -> Size -> m (Maybe Ptr)
    -- TODO free should return an error or (Nothing, ok, anything else?), is Maybe sufficient for that?
    free    :: (ManagerSig sig m, Has (State PMap) sig m) => a -> Ptr -> m (Maybe Ptr)

class Pointer a where
  fromOffs :: Integral b => b -> a
  toOffs :: Integral b => a -> b


instance Pointer Int where
    fromOffs = undefined
    toOffs = undefined


instance Allocator ProcessId where
    alloc pid sz = do
        -- TODO instead of 128 should be frame size from Env
        let np = fromCount (unSize sz) `div` 1024

        -- allocating np number of pages
        ps <- mapM allocPage [pid | _ <- [1..np]]
        -- add pages to pid map
        modify @PMap $ adjust (<> ps) pid
        -- in case at least one page in `ps` exists then return Ptr to it, otherwise Nothing
        let fp = find (const True) ps
        return $ case fp of
            (Just Page { frId }) -> Just $ fromOffs $ unFid frId
            Nothing -> Nothing

    realloc pid pt sz = free pid pt >> alloc pid sz

    free pid ptr = do
        modify @PMap $ adjust (Foundation.filter (\Page { frId } -> unFid frId /= toOffs ptr)) pid

        return Nothing