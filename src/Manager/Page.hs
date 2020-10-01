module Manager.Page
  ( PageTable
  , Page (..)
  , Pages (..)
  , findPageToUnload
  ) where 

import Foundation
import Foundation.Collection
import Control.Effect.State
import Manager.Types

data Page (a :: MemType) = Page
            { frId :: FrameId a
            , age :: Age -- internal age of page, used in alg
            , wBit :: Bool -- If some process is writing this page now, then no other process can write it
            , rBit :: Bool -- -//- (writing -> reading)
            , pId :: ProcessId
            } deriving (Eq, Show)

type PageTable = ([Page 'Ram], [Page 'Swap])

type PagesSig sig m = (Has (State PageTable) sig m)

class Pages a where
  createPage :: PagesSig sig m => FrameId a -> ProcessId -> m (Page a)
  deletePage :: PagesSig sig m => Page a -> m ()
  getPage :: PagesSig sig m => FrameId a -> m (Maybe (Page a))
  movePage :: PagesSig sig m => Page a -> FrameId a -> m ()

-- Used only in situation, where no free Ram pages exists
-- which means, that ram is not empty
findPageToUnload :: PagesSig sig m => m (Page 'Ram)
findPageToUnload = do
  (ram, _) <- get @PageTable
  return . foldl1' (\acc el -> if age el < age acc then el else acc) $ nonEmpty_ ram

instance Pages 'Ram where
  createPage fid pid = do
    let np = Page fid (Age 100) False False pid
    modify @PageTable $ first (np:)
    return np

  deletePage page = do
    modify @PageTable $ first (filter (/= page))

  getPage fid = do
    (ram, _) <- get @PageTable
    let frM = find (\(Page { frId }) -> if frId == fid then True else False) ram
    return frM
  
  movePage p@(Page { pId }) fid = do
    np <- createPage (Fid $ unFid fid) pId
    modify @PageTable $ bimap (filter (/= p)) (np:)

instance Pages 'Swap where
  createPage fid pid = do
    let np = Page fid (Age 0) False False pid
    modify @PageTable $ second (np:)
    return np

  deletePage page = do
    modify @PageTable $ second (filter (/= page))

  getPage fid = do
    (_, sw) <- get @PageTable
    let frM = find (\(Page { frId }) -> if frId == fid then True else False) sw
    return frM
  
  movePage p@(Page { pId }) fid = do
    np <- createPage (Fid $ unFid fid) pId
    modify @PageTable $ bimap (np:) (filter (/= p))