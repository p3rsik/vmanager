{-# LANGUAGE DataKinds #-}

module Manager.FrameSpec (spec) where

import Foundation
import Foundation.Collection
import Control.Carrier.State.Strict
import Control.Algebra
import Test.Hspec
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Monadic as QM
import Manager.Types
import Manager.Frame

idToOffToId :: Q.NonNegative Int -> Bool
idToOffToId (Q.NonNegative x) = (idToOff . offToId $ Offset x) == (Offset x)

offToIdToOff :: Q.NonNegative Int -> Bool
offToIdToOff (Q.NonNegative x) = (offToId . idToOff $ Fid x) == (Fid x)

frameNumber = 32

startingState :: FrameTable
startingState = FT rfo ram sfo sw where
    rfo = [Offset x | x <- [0..frameNumber]]
    ram = nonEmpty_ [Frame (Fid x) [0..64] | x <- [0..frameNumber]]
    sfo = [Offset x | x <- [0..frameNumber]]
    sw = nonEmpty_ [Frame (Fid x) [0..64] | x <- [0..frameNumber]]

run' = run . evalState startingState 

getFrameTest :: Q.Property
getFrameTest = Q.forAll (Q.choose (0, frameNumber)) $ \x ->
    let res = run' $ getFrame (Fid x :: FrameId 'Ram)
        (FT _ ram _ _) = startingState
        res' = nonEmpty_ . filter (\f -> x == (unFid $ frameId f)) $ getNonEmpty ram
        in res == head res'

spec :: Spec
spec = do 
    describe "idToOff and offToId roundabout" $ do 
        it "idToOff . offToId == id" $ Q.property $ idToOffToId
        it "offToId . idToOff == id" $ Q.property $ offToIdToOff
        it "getFrame tests" $ Q.property $ getFrameTest