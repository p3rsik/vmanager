module Manager.Page
  ( PageTable
  , Page (..)
  , Pages (..)
  ) where 

import Foundation
import Control.Effect.State
import Manager.Types

data Page (a :: MemType) = Page
            { frId :: FrameId
            , age :: Age -- internal age of page, used in alg
            , memType :: MemType
            , wBit :: Bool -- If some process is writing this page now, then no other process can write it
            , rBit :: Bool -- -//- (writing -> reading)
            , pId :: ProcessId
            } deriving (Eq, Show)

type PageTable = ([Page 'Ram], [Page 'Swap])

type PagesSig sig m = (Has (State PageTable) sig m)

class Pages a where
  createPage :: PagesSig sig m => FrameId -> ProcessId -> m (Page a)
  deletePage :: PagesSig sig m => Page a -> m ()
  getPage :: PagesSig sig m => FrameId -> m (Maybe (Page a))
  movePage :: PagesSig sig m => Page a -> FrameId -> m ()

instance Pages 'Ram where
  createPage fid pid = do
    let np = Page fid (Age 100) Ram False False pid
    modify @PageTable $ first (np:)
    return np

  deletePage page = do
    modify @PageTable $ first (filter (/= page))

  getPage fid = do
    (ram, _) <- get @PageTable
    let frM = find (\(Page { frId }) -> if frId == fid then True else False) ram
    return frM
  
  movePage p@(Page { pId }) fid = do
    let np = Page fid (Age 0) Swap False False pId
    modify @PageTable $ bimap (filter (/= p)) (np:)

instance Pages 'Swap where
  createPage fid pid = do
    let np = Page fid (Age 100) Swap False False pid
    modify @PageTable $ second (np:)
    return np

  deletePage page = do
    modify @PageTable $ second (filter (/= page))

  getPage fid = do
    (_, sw) <- get @PageTable
    let frM = find (\(Page { frId }) -> if frId == fid then True else False) sw
    return frM
  
  movePage p@(Page { pId }) fid = do
    let np = Page fid (Age 0) Ram False False pId
    modify @PageTable $ bimap (np:) (filter (/= p))