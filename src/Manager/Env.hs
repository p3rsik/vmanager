module Manager.Env
  ( Env (..)
  ) where

import Foundation

import Manager.Frame (Frame)
import Manager.Types

data Env = Env { memSize :: CountOf Word8
               , ramSize :: CountOf (Frame 'Ram)
               , swapSize :: CountOf (Frame 'Swap)
               }
