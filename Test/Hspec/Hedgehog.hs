{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Hedgehog
  ( prop
  , evalHedgehogExample
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Hedgehog (Property, Size (..), Test)
import qualified Hedgehog
import qualified Hedgehog.Internal.Property as HP
import qualified Hedgehog.Internal.Runner as HR
import qualified Hedgehog.Internal.Seed as HS
import           Hedgehog.Internal.Report (Report (..), Status (..))

import           Test.Hspec.Core.Spec (Arg, Example (..), Spec)
import           Test.Hspec.Core.Spec (ActionWith, Params, ProgressCallback, Result)
import qualified Test.Hspec.Core.Spec as Hspec

prop :: String -> Test IO () -> Spec
prop str = Hspec.it str . Hedgehog.property


instance Example Property where
  type Arg Property = ()
  evaluateExample = evalHedgehogExample


evalHedgehogExample :: MonadIO m
                    => Property -> Params -> (ActionWith (Arg Property) -> IO ()) -> ProgressCallback -> m Result
evalHedgehogExample p params _ reportProgress = do
  seed <- HS.random
  rep <- liftIO $ HR.checkReportNoCatch HP.defaultConfig size seed (HP.propertyTest p) (reportProgress . reporter)
  case reportStatus rep of
    Waiting -> error "waiting"
    Running -> error "running"
    Shrinking _ -> error "shrinking"
    Failed _ -> pure $ Hspec.Failure Nothing (Hspec.Reason "condition is false")
    GaveUp -> error "running"
    OK -> pure Hspec.Success
  where
    size = Size $ Hspec.paramsSmallCheckDepth params


    reporter :: Report -> (Int, Int)
    reporter r =
      (fromIntegral (reportTests r), Hspec.paramsSmallCheckDepth params)
