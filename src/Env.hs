module Env
  ( Env (..)
  ) where

import Foundation

import Frame (Frame)
import Types

data Env = Env { memSize :: CountOf Word8
               , ramSize :: CountOf (Frame 'Ram)
               , swapSize :: CountOf (Frame 'Swap)
               }
