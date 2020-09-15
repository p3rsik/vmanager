{-# LANGUAGE FlexibleContexts #-}

module Manager
    ( 
    ) where

import Foundation
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef
import Data.Bits

import Page
import Frame

data Env = Env
           { pageTable :: IORef PageTable
           , frameTable :: IORef FrameTable
           }

-- TODO:
-- Add custom error type
-- Implement load page

modifyPT :: (MonadError String m, MonadReader Env m, MonadIO m) => Env -> (PageTable -> PageTable) -> m ()
modifyPT env action = liftIO $ modifyIORef' (pageTable env) action
modifyFT :: (MonadError String m, MonadReader Env m, MonadIO m) => Env -> (FrameTable -> FrameTable) -> m ()
modifyFT env action = liftIO $ modifyIORef' (frameTable env) action
getPageTable :: (MonadError String m, MonadReader Env m, MonadIO m) => Env -> m PageTable
getPageTable env = liftIO . readIORef $ pageTable env
getFrameTable :: (MonadError String m, MonadReader Env m, MonadIO m) => Env -> m FrameTable
getFrameTable env = liftIO . readIORef $ frameTable env

-- Find free frame in RAM(if available), if not, unload frame from RAM to SWAP and use it
createPage :: (MonadError String m, MonadReader Env m, MonadIO m) => ProcessId -> m ()
createPage pid = do
  env <- ask
  pt <- getPageTable env
  ft <- getFrameTable env
  -- setting page counter(age) to 100 so it won't go into SWAP right after creation
  let (t, ni) = case getFreeRamPage ft of
                  Just x -> (True, x)
                  Nothing -> (False, 0)
  unless t $ do
    let pu = foldl' (\acc el -> if pcounter acc < pcounter el then acc else el) (Page 0 0 (maxBound :: Word) False False False 0) pt
    unloadPage pu `catchError` (\_ -> throwError "Can't create page")

  let np = Page ni (fromIntegral . fromCount $ length pt) 100 True False False pid
  modifyFT env popFreeRamPage
  modifyPT env (\pt' -> (np:pt'))

-- Find corresponding frame and mark it free, then delete page from pool of pages
deletePage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
deletePage page = do
  env <- ask
  modifyPT env $ filter (\el -> el /= page)
  modifyFT env $ (\(FrameTable xs fs ys zs) -> FrameTable (Page.frameId page:xs) fs ys zs)

-- Load page from SWAP to RAM
loadPage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
loadPage = undefined

-- Unload page from RAM to SWAP
unloadPage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
unloadPage page@(Page _ po c _ w r pid) = do
  unless (inRam page) . throwError $ "Trying to unload page that is not in RAM" <> show page
  env <- ask
  ft <- getFrameTable env
  let ind = Page.frameId page
  let (t, nfi) = case getFreeSwapPage ft of
                   Just x -> (True, x)
                   Nothing -> (False, 0)
  unless t . throwError $ "Can't unload " <> show page <> ": Not enough memory in SWAP"

  modifyFT env $ (\(FrameTable xs ys fs zs) -> FrameTable (ind:xs) ys fs zs)
  modifyFT env popFreeSwapPage

  modifyPT env $ fmap (\p -> if page == p then Page nfi po c False w r pid else p)

writePage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
writePage page@(Page fi po c ir _ r pid) = do
  env <- ask
  modifyPT env $ fmap (\p -> if page == p then Page fi po c ir True r pid else p)
  ageWorld
  modifyPT env $ fmap (\p -> if page == p then Page fi po c ir False r pid else p)

ageWorld :: (MonadError String m, MonadReader Env m, MonadIO m) => m ()
ageWorld = do
  env <- ask
  liftIO . modifyIORef' (pageTable env) $ fmap agePage
  where
    agePage :: Page -> Page
    agePage (Page fi po c ir w r pid) = let c' = if r
                                                 then setBit (shiftR c 1) (finiteBitSize c - 1)
                                                 else shiftR c 1 in
      Page fi po c' ir w r pid

readPage :: (MonadError String m, MonadReader Env m, MonadIO m) => Page -> m ()
readPage page@(Page fi po c ir w _ pid) = do
  env <- ask
  modifyPT env $ fmap (\p -> if page == p then Page fi po c ir w True pid else p)
  ageWorld
  modifyPT env $ fmap (\p -> if page == p then Page fi po c ir w False pid else p)
