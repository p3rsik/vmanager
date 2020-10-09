module Allocator.Process where

import Foundation
import Manager
import Control.Effect.State

newtype Mem = Mem [Page 'Ram] deriving Eq

-- list of all processes
type PTable = [ProcessId]
type ProcSig sig m = (Has (State PTable) sig m)


-- add process to PTable list
createProcess :: (ProcSig sig m) => ProcessId -> m ()
createProcess pid = do
    modify @PTable (pid:)

-- delete process from PTable list
deleteProcess :: (ProcSig sig m) => ProcessId -> m ()
deleteProcess pid = do
    modify @PTable $ filter (/= pid)