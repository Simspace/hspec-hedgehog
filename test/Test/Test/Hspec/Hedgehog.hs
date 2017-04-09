{-# LANGUAGE TypeFamilies #-}
module Test.Test.Hspec.Hedgehog
  ( hedgehogSpec
  ) where

import           Hedgehog (Property)
import qualified Hedgehog
import qualified Hedgehog.Gen as HG
import qualified Hedgehog.Range as HR

import           Test.Hspec.Hedgehog

import           Test.Hspec (Spec, errorCall, shouldReturn, shouldThrow)
import qualified Test.Hspec.Core.Spec as Hspec


hedgehogSpec :: Spec
hedgehogSpec = do
  Hspec.describe "evaluateExample" $ do
    Hspec.context "Property" $ do
      Hspec.it "returns Success if property holds" $ do
        evaluateExample (Hedgehog.property $ Hedgehog.assert True) `shouldReturn` Hspec.Success

      Hspec.it "returns Fail if property does not hold" $ do
        evaluateExample (Hedgehog.property $ Hedgehog.assert False) `shouldReturn` Hspec.Failure Nothing (Hspec.Reason "condition is false")

      Hspec.it "shows what falsified it" $ do
        evaluateExample notTwo `shouldReturn` Hspec.Failure Nothing (Hspec.Reason "condition is false")

      Hspec.it "propagates exceptions" $ do
        evaluateExample (error "foobar") `shouldThrow` errorCall "foobar"
  where
    evaluateExample :: Property -> IO Hspec.Result
    evaluateExample e = hedgehogEvalExample e undefined undefined undefined

    notTwo :: Property
    notTwo = Hedgehog.property $ do
      x <- Hedgehog.forAll $ HG.integral (HR.linear 0 10)
      Hedgehog.assert $ x /= (2 :: Int)
