module Env
  ( Env (..)
  ) where

import Foundation

import Frame (Frame)
import Types (RAM, SWAP)

data Env = Env { memSize :: CountOf Word8
               , ramSize :: CountOf (Frame RAM)
               , swapSize :: CountOf (Frame SWAP)
               }
