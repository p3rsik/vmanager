module Allocator.Process where

import Foundation
import Manager
import Control.Effect.State

newtype Pid = Pid Int deriving Eq
newtype Mem = Mem [Page 'Ram] deriving Eq
data Process = Process { pid :: Pid } deriving Eq


-- list of all processes
type PTable = [Process]
type ProcSig sig m = (Has (State PTable) sig m)


-- add process to PTable list
createProcess :: (ProcSig sig m) => Pid -> m ()
createProcess pid = do
    modify @PTable ((Process pid):)

-- delete process from PTable list
deleteProcess :: (ProcSig sig m) => Pid -> m ()
deleteProcess pid = do
    modify @PTable $ filter (/= (Process pid))


