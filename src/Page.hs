module Page
  ( PageTable
  , ProcessId
  , Page (..)
  ) where 

import Foundation

type ProcessId = Int

data Page = Page
            { frameId :: Word
            , pageOffset :: Word
            , pcounter :: Word
            , inRam :: Bool
            , wBit :: Bool -- If some process is writing this page now, then no other process can write it
            , rBit :: Bool -- -//- (writing -> reading)
            , processId :: ProcessId
            } deriving (Eq, Show)
 
type PageTable = [Page] -- TODO swap List to Vector
