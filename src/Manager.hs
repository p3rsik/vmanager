module Manager
  ( loadPage
  , unloadPage
  , writePage
  , readPage
  , module Manager.Types
  , module Manager.Page
  , module Manager.Frame
  , module Manager.Env
  )
where

import Data.Bits
import GHC.Types

import Foundation
import Foundation.Collection
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
createPage :: ManagerSig sig m => ProcessId -> m (Maybe (Page 'Ram))
createPage pid = undefined

-- Find corresponding frame and mark it free, then delete page from pool of pages
deletePage :: ManagerSig sig m => Page a -> m ()
deletePage p = undefined

-- Used only in situation, where no free Ram pages exists
-- which means, that ram array is not empty
findPageToUnload :: ManagerSig sig m => m (Page 'Ram)
findPageToUnload = do
  (ram, _) <- get @PageTable
  return . foldl1' (\acc el -> if age el < age acc then el else acc) $ nonEmpty_ ram

-- Load page from SWAP to RAM
loadPage :: ManagerSig sig m => Page 'Swap -> m ()
loadPage p@(Page { frId, pId }) = do
  -- Get free RAM Frame
  offM <- getFreeFrameOffset @'Ram
  case offM of
    -- If there is a free RAM page, use it
    Just off -> do 
      -- get offset corresponding to the swap frame that needs to be freed
      let swapOff = idToOff frId
      -- Free SWAP Frame
      setFrameFree @'Swap swapOff

      -- get frameId corresponding to the free frame
      let fId = offToId off
      let np = Page fId (Age 100) Ram False False pId

      -- save memory
      copyMem p np

      -- replace existing page with new page corresponding to the found Frame
      modify @PageTable $ bimap (np:) (filter (/= p))

    -- If there is no free RAM page available, then find one, that can be unloaded
    Nothing -> do
      -- fId - Ram frame id(and offset) that would be free after page unloading
      pu@(Page { frId = fId }) <- findPageToUnload
      -- mark Swap Frame that we're unloading from as free
      setFrameFree @'Swap $ idToOff frId
      -- if there are no free swap frames, this ^ can give us one frame that we need
      unloadPage pu
      let np = Page fId (Age 100) Ram False False pId
      copyMem p np
      modify @PageTable $ first (np:)

-- Copy memory from one page to another
copyMem :: ManagerSig sig m => Page a -> Page b -> m ()
copyMem f t = do
  env <- ask @Env
  mem <- readPage f (Offset 0) $ memSize env
  writePage t (Offset 0) mem

-- Unload page from RAM to SWAP
unloadPage :: ManagerSig sig m => Page 'Ram -> m ()
unloadPage p@(Page {frId, pId}) = do
  offM <- getFreeFrameOffset @'Swap
  case offM of
    Just off -> do
      setFrameFree @'Ram $ idToOff frId
      let np = Page (offToId off) (Age 100) Swap False False pId

      -- save memory
      copyMem p np

      -- delete current page from RAM pages and add new page to the SWAP pages
      modify @PageTable $ bimap (filter (/= p)) (np:)

    -- if no free Swap frames exists -> throw error
    Nothing -> throwError NoFreeSwapFrames

-- Write memory to the page
writePage :: ManagerSig sig m => Page a -> Offset Word8 -> [Word8] -> m ()
writePage (Page { frId, memType }) off mem = do
  case memType of
    Ram -> do
      f <- getFrame @'Ram frId
      writeFrame f off mem
    Swap -> do
      f <- getFrame @'Swap frId
      writeFrame f off mem

-- Read memory from the page
readPage :: ManagerSig sig m => Page a -> Offset Word8 -> CountOf Word8 -> m ([Word8])
readPage p off count = undefined

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