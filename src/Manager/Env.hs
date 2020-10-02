module Manager.Env
  ( Env (..)
  ) where

import Foundation

import Manager.Frame (Frame)
import Manager.Types

data Env = Env { memSize :: CountOf Word8 -- Size of memory in one Frame
               , ramSize :: CountOf (Frame 'Ram) -- Number of Ram Frame's
               , swapSize :: CountOf (Frame 'Swap) -- Number of Swap Frame's
               }
