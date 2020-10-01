module Manager.TypesSpec (spec) where

import Foundation
import Test.Hspec
import Manager.Types

spec :: Spec
spec = do 
    describe "Manager.Types module" $ do 
        describe "idToOff and offToId roundabout" $ do
            it "idToOff . offToId == id" $ do
                pending
            it "offToId . idToOff == id" $ do
                pending
