{-# LANGUAGE NamedFieldPuns #-}

module Page
  ( PageTable
  , Page (..)
  ) where 

import Foundation
import Control.Effect.State

import Types
import Frame (Frame)

data Page a = Page
            { frId :: FrameId
            , age :: Age -- internal age of page, used in alg
            , wBit :: Bool -- If some process is writing this page now, then no other process can write it
            , rBit :: Bool -- -//- (writing -> reading)
            , pId :: ProcessId
            } deriving (Eq, Show)

type PageTable = ([Page RAM], [Page SWAP])