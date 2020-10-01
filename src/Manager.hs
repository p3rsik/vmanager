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
import GHC.Types

import Foundation
import Control.Effect.State
import Control.Effect.Catch
import Control.Effect.Throw
import Control.Effect.Reader

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
                        , Has (Reader Env) sig m
                        , Has (Throw ManagerError) sig m
                        , Has (Catch ManagerError) sig m
                        )

-- Find free RAM frame (if available), if not, unload frame from RAM to SWAP and use it
allocPage :: ManagerSig sig m => ProcessId -> m (Maybe (Page 'Ram))
allocPage pid = undefined

-- Find corresponding frame and mark it free, then delete page from pool of pages
freePage :: ManagerSig sig m => Page a -> m ()
freePage p = undefined

-- Load page from SWAP to RAM
moveToRam :: ManagerSig sig m => Page 'Swap -> m ()
moveToRam p@(Page { frId, pId }) = do
  -- Get free RAM Frame
  offM <- getFreeFrameOffset @'Ram
  case offM of
    -- If there is a free RAM page, use it
    Just off -> do 
      -- Delete page and free corresponding frame
      deletePage p
      setFrameFree frId

      -- create new page and mark corresponding frame not free
      np <- createPage (offToId off) pId
      setFrameNotFree (offToId off)

      -- move memory
      copyMem p np
    -- If there is no free RAM page available, then find one, that can be unloaded
    Nothing -> do
      pu@(Page { frId = fId }) <- findPageToUnload
      moveToSwap pu

      np <- createPage fId pId
      -- mark Swap Frame that we're unloading from as free
      setFrameFree frId
      copyMem p np
      setFrameNotFree fId

-- Copy memory from one page to another
copyMem :: ManagerSig sig m => Page a -> Page b -> m ()
copyMem f t = do
  env <- ask @Env
  mem <- readMem f (Offset 0) $ memSize env
  writeMem t (Offset 0) mem

-- Unload page from RAM to SWAP
moveToSwap :: ManagerSig sig m => Page 'Ram -> m ()
moveToSwap p@(Page {frId, pId}) = do
  offM <- getFreeFrameOffset @'Swap
  case offM of
    Just off -> do
      -- Create new page and mark corresponding frame NOT free
      setFrameNotFree $ offToId off
      np <- createPage (offToId off) pId

      -- save memory
      copyMem p np

      -- delete current page from RAM pages and add new page to the SWAP pages
      deletePage p
      setFrameFree frId

    -- if no free Swap frames exists -> throw error
    Nothing -> throwError NoFreeSwapFrames

-- Write memory to the page
writeMem :: ManagerSig sig m => Page a -> Offset Word8 -> [Word8] -> m ()
writeMem (Page { frId, memType }) off mem = do
  case memType of 
    Ram -> do 
      f <- getFrame @'Ram (Fid $ unFid frId)
      writeFrame f off mem
    Swap -> do
      f <- getFrame @'Swap (Fid $ unFid frId)
      writeFrame f off mem

-- Read memory from the page
readMem :: ManagerSig sig m => Page a -> Offset Word8 -> CountOf Word8 -> m ([Word8])
readMem p off count = undefined

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