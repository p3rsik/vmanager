module Allocator.Allocator where

import Control.Effect.Error
import Control.Effect.State
import Control.Monad
import Data.HashMap.Lazy
import Env
import Foundation
import Manager

-- map of processes to its pages
type PMap = HashMap ProcessId [Page 'Ram]

type Ptr = Int

data AllocError = OutOfMemory deriving (Eq, Ord)

type AllocSig sig m =
  ( Has (State PMap) sig m,
    Has (Throw AllocError) sig m,
    Has (Catch AllocError) sig m
  )

-- TODO mock of env, actually should be exported from somewhere
env :: Env
env = Env 1024 0 0

class Allocator a where
  alloc :: (ManagerSig sig m, Has (State PMap) sig m) => a -> CountOf Word8 -> m (Maybe Ptr)
  free :: (ManagerSig sig m, Has (State PMap) sig m) => a -> m (Maybe a)

class Pointer a where
  fromOffs :: (Monad m) => PMap -> ProcessId -> Integer -> m a

instance Pointer Int where
  fromOffs pm pid np = do
    return $ (fromCount (length pn) - fromIntegral np) * fromCount (memSize env)
    where
      pn = fromMaybe [] $ lookup pid pm

instance Allocator ProcessId where
  alloc pid sz = do
    case sz of
      0 -> return Nothing

    -- number of new pages to alloc
    let npNotAligned = fromCount sz `div` fromCount (memSize env)
    let np = npNotAligned + align
          where
            align =
              if npNotAligned < fromCount sz
                then 1
                else 0

    -- allocating np number of pages
    ps <- mapM allocPage [pid | _ <- [1 .. np]]
    -- add pages to pid map
    modify @PMap $ adjust (<> ps) pid
    -- in case at least one page in `ps` exists then return Ptr to it, otherwise Nothing

    let ptr = (fromCount (length ps) - np) * fromCount (memSize env)

    -- if there was allocated zero pages - return nothing
    return $ case find (const True) ps of
      (Just _) -> Just ptr
      Nothing -> Nothing

  free pid = do
    modify @PMap $ delete pid
    return Nothing