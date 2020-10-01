module Manager.TypesSpec (spec) where

import Foundation
import Test.Hspec
-- import Test.QuickCheck.Modifiers
import Test.QuickCheck
import Manager.Types

idToOffToId :: NonNegative Int -> Bool
idToOffToId (NonNegative x) = (idToOff . offToId $ Offset x) == (Offset x)

offToIdToOff :: NonNegative Int -> Bool
offToIdToOff (NonNegative x) = (offToId . idToOff $ Fid x) == (Fid x)

spec :: Spec
spec = do 
    describe "idToOff and offToId roundabout" $ do 
        it "idToOff . offToId == id" $ 
            property $ idToOffToId
        it "offToId . idToOff == id" $
            property $ offToIdToOff
