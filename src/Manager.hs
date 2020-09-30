{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Manager
  ( createPage
  , deletePage
  , loadPage
  , unloadPage
  , writePage
  , readPage
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

import Frame
import Env
import Page
import Types

data ManagerError = CantCreatePage 
                  | NoFreeSwapFrames
                  | FuckYou 
                  deriving (Eq, Ord)

type Manager sig m = (Has (State FrameTable) sig m,
                      Has (State PageTable) sig m,
                      Has (Reader Env) sig m,
                      Has (Throw ManagerError) sig m,
                      Has (Catch ManagerError) sig m)

-- Find free RAM frame (if available), if not, unload frame from RAM to SWAP and use it
createPage :: Manager sig m => ProcessId -> m (Maybe (Page RAM))
createPage pid = undefined

-- Find corresponding frame and mark it free, then delete page from pool of pages
deletePage :: Manager sig m => Page a -> m ()
deletePage p = undefined

-- Used only in situation, where no free Ram pages exists
-- which means, that ram array is not empty
findPageToUnload :: Manager sig m => m (Page RAM)
findPageToUnload = do
  (ram, _) <- get @PageTable
  return . foldl1' (\acc el -> if age el < age acc then el else acc) $ nonEmpty_ ram

-- Load page from SWAP to RAM
loadPage :: Manager sig m => Page SWAP -> m ()
loadPage p@(Page {frId, pId}) = do
  -- Get free RAM Frame
  offM <- getFreeFrameOffset @RAM
  case offM of
    -- If there is a free RAM page, use it
    Just off -> do 
      -- get offset corresponding to the swap frame that needs to be freed
      let swapOff = idToOff frId
      -- Free SWAP Frame
      setFrameFree @SWAP swapOff

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
      pu@(Page {frId = fId}) <- findPageToUnload
      -- mark Swap Frame that we're unloading from as free
      setFrameFree @SWAP $ idToOff frId
      -- if there are no free swap frames, this ^ can give us one frame that we need
      unloadPage pu
      let np = Page fId (Age 100) Ram False False pId
      copyMem p np
      modify @PageTable $ first (np:)

-- Copy memory from one page to another
copyMem :: Manager sig m => Page a -> Page b -> m ()
copyMem f t = do
  env <- ask @Env
  mem <- readPage f (Offset 0) $ memSize env
  writePage t (Offset 0) mem

-- Unload page from RAM to SWAP
unloadPage :: Manager sig m => Page RAM -> m ()
unloadPage p@(Page {frId, pId}) = do
  offM <- getFreeFrameOffset @SWAP
  case offM of
    Just off -> do
      setFrameFree @RAM $ idToOff frId
      let np = Page (offToId off) (Age 100) Swap False False pId

      -- save memory
      copyMem p np

      -- delete current page from RAM pages and add new page to the SWAP pages
      modify @PageTable $ bimap (filter (/= p)) (np:)

    -- if no free Swap frames exists -> throw error
    Nothing -> throwError NoFreeSwapFrames

-- Write memory to the page
writePage :: Manager sig m => Page a -> Offset Word8 -> [Word8] -> m ()
writePage (Page { frId, memType }) off mem = do
  case memType of
    Ram -> do
      f <- getFrame @RAM frId
      writeFrame f off mem
    Swap -> do
      f <- getFrame @SWAP frId
      writeFrame f off mem

-- Read memory from the page
readPage :: Manager sig m => Page a -> Offset Word8 -> CountOf Word8 -> m ([Word8])
readPage p off count = undefined


-- createPage :: (MonadError String m, MonadReader Env m, MonadIO m) => ProcessId -> m ()
-- createPage pid = do
  -- env <- ask
  -- pt <- getPageTable env
  -- ft <- getFrameTable env
  -- -- setting page counter(age) to 100 so it won't go into SWAP right after creation
  -- let (t, ni) = case getFreeRamPage ft of
        -- Just x -> (True, x)
        -- Nothing -> (False, 0)
  -- unless t $ do
    -- let pu = foldl' (\acc el -> if pcounter acc < pcounter el then acc else el) (Page 0 (maxBound :: Word) False False False 0) pt
    -- unloadPage pu `catchError` (\_ -> throwError "Can't create page")

  -- let np = Page ni 100 True False False pid
  -- modifyFT env popFreeRamPage
  -- modifyPT env $ const (np : pt)

-- deletePage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
-- deletePage page = do
  -- env <- ask
  -- modifyPT env $ filter (/= page)
  -- modifyFT env (\(FrameTable xs fs ys zs) -> FrameTable (Page.frameId page : xs) fs ys zs)

-- writePage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
-- writePage page@(Page fi c ir _ r pid) = do
  -- env <- ask
  -- modifyPT env $ fmap (\p -> if page == p then Page fi c ir True r pid else p)
  -- ageWorld
  -- modifyPT env $ fmap (\p -> if page == p then Page fi c ir False r pid else p)

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

-- readPage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
-- readPage page@(Page fi c ir w _ pid) = do
  -- env <- ask
  -- modifyPT env $ fmap (\p -> if page == p then Page fi c ir w True pid else p)
  -- ageWorld
  -- modifyPT env $ fmap (\p -> if page == p then Page fi c ir w False pid else p)
