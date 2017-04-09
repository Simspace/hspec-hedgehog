{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.Hedgehog
  ( prop
  , hedgehogEvalExample
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad.IO.Class (MonadIO)
import           Hedgehog (Property, Test)
import qualified Hedgehog

import           Test.Hspec.Core.Spec (Arg, Example (..), Spec)
import qualified Test.Hspec.Core.Spec as Hspec

prop :: String -> Test IO () -> Spec
prop str = Hspec.it str . Hedgehog.property


instance Example Property where
  type Arg Property = ()
  evaluateExample = hedgehogEvalExample


hedgehogEvalExample :: MonadIO m => Property -> a -> b -> c -> m Hspec.Result
hedgehogEvalExample p _ _ _reportProgress = do
  r <- Hedgehog.check p
  if r
    then pure Hspec.Success
    else pure $ Hspec.Failure Nothing (Hspec.Reason "condition is false")

