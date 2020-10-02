module Manager
  ( allocPage
  , freePage
  , writeMem
  , readMem
  , moveToRam
  , moveToSwap
  , module Manager.Types
  , module Manager.Page
  , module Manager.Frame
  , module Manager.Env
  )
where

-- import Data.Bits
import Foundation
import Control.Effect.State
import Control.Effect.Catch
import Control.Effect.Throw

import Manager.Frame
import Manager.Env
import Manager.Page
import Manager.Types

data ManagerError = CantCreatePage 
                  | NoFreeSwapFrames
                  | FuckYou 
                  deriving (Eq, Ord)

type ManagerSig sig m = ( Has (State FrameTable) sig m
                        , Has (State PageTable) sig m
                        , Has (Throw ManagerError) sig m
                        , Has (Catch ManagerError) sig m
                        )


-- Find free RAM frame (if available), if not, unload frame from RAM to SWAP and use it
allocPage :: ManagerSig sig m => ProcessId -> m (Page 'Ram)
allocPage pid = do
  -- get offset of any free frame
  offM <- getFreeFrameOffset @'Ram
  case offM of
    -- in case free frame exists, then alloc a new page
    Just off -> do
      np <- createPage (offToId off) pid
      return np

    -- otherwise move specific page to swap and alloc a new page
    Nothing -> do
      -- unload a page to swap and create a new one instead
      p@(Page { frId }) <- findPageToUnload
      moveToSwap p

      np <- createPage frId pid
      setFrameNotFree frId

      return np


-- Find corresponding frame and mark it free, then delete page from pool of pages
freePage :: (Pages a, Frames a, ManagerSig sig m) => Page a -> m ()
freePage p@(Page { frId }) = do
  deletePage p
  setFrameFree frId


-- Load page from SWAP to RAM
moveToRam :: ManagerSig sig m => Page 'Swap -> m ()
moveToRam p@(Page { pId }) = do
  np <- allocPage pId
  copyMem p np
  freePage p


-- Copy memory from one page to another
copyMem :: (Pages a, Frames a, Pages b, Frames b, ManagerSig sig m) => Page a -> Page b -> m ()
copyMem (Page { frId }) t = do
  (Frame _ mem) <- getFrame frId
  writeMem t (Offset 0) mem


-- Unload page from RAM to SWAP
moveToSwap :: ManagerSig sig m => Page 'Ram -> m ()
moveToSwap p@(Page { pId }) = do
  offM <- getFreeFrameOffset @'Swap
  case offM of
    Just off -> do
      -- Create new page and mark corresponding frame NOT free
      np <- createPage (offToId off) pId
      -- save memory
      copyMem p np
      freePage p
    -- if no free Swap frames exists -> throw error
    Nothing -> throwError NoFreeSwapFrames


-- Write memory to the page
writeMem :: (Pages a, Frames a, ManagerSig sig m) => Page a -> Offset Word8 -> [Word8] -> m ()
writeMem (Page { frId }) off mem = do
  f <- getFrame frId
  writeFrame f off mem


-- Read memory from the page
readMem :: (Pages a, Frames a, ManagerSig sig m) => Page a -> Offset Word8 -> CountOf Word8 -> m ([Word8])
readMem (Page { frId }) off count = do
  f <- getFrame frId
  readFrame f off count


-- ageWorld :: (MonadError String m, MonadReader Env m, MonadIO m) => m ()
-- ageWorld = do
  -- env <- ask
  -- liftIO . modifyIORef' (pageTable env) $ fmap agePage
  -- where
    -- agePage :: Page -> Page
    -- agePage (Page fi c ir w r pid) =
      -- let c' =
            -- if r
              -- then setBit (shiftR c 1) (finiteBitSize c - 1)
              -- else shiftR c 1
       -- in Page fi c' ir w r pid