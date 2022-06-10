{-# LANGUAGE BangPatterns        #-}

module HaskellWorks.Control.Monad.LazySpec (spec) where

import Control.Monad.IO.Class
import Hedgehog
import Prelude hiding (log)
import Test.Hspec

import qualified Control.Concurrent.STM           as STM
import qualified HaskellWorks.Control.Monad.Lazy  as LZ
import qualified HaskellWorks.Hspec.Hedgehog      as H

{- HLINT ignore "Redundant do" -}

spec :: Spec
spec = describe "HaskellWorks.Control.Monad.LazySpec" $ do
  it "Be able to load file into Vector" $ H.requireTest $ do
    tvLog <- liftIO $ STM.newTVarIO []
    as <- liftIO $ LZ.replicateM 3 $ STM.atomically $ STM.modifyTVar tvLog (():)
    liftIO (STM.atomically (STM.readTVar tvLog)) >>= do \log -> log === []
    let !(():_) = as
    liftIO (STM.atomically (STM.readTVar tvLog)) >>= do \log -> log === [()]
    let !(():():_) = as
    liftIO (STM.atomically (STM.readTVar tvLog)) >>= do \log -> log === [(), ()]
    let !(():():():_) = as
    liftIO (STM.atomically (STM.readTVar tvLog)) >>= do \log -> log === [(), (), ()]
    as === [(), (), ()]
