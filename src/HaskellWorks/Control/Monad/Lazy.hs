{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Control.Monad.Lazy
  ( replicateM
  , sequenceM
  , unfoldrM
  , traverseM
  , forM
  , forceM
  ) where

import Control.DeepSeq
import Control.Monad ((<$!>))
import Control.Monad.IO.Unlift

import qualified System.IO.Unsafe as IO

replicateM :: MonadUnliftIO m => Int -> m a -> m [a]
replicateM n f = sequenceM (replicate n f)

sequenceM :: MonadUnliftIO m => [m a] -> m [a]
sequenceM as = do
  f <- askUnliftIO
  liftIO $ sequenceIO (fmap (unliftIO f) as)

-- | Generates a lazy list of values that are produced by a given monadic function.
--
-- This function is intended to be like the "standard" 'unfoldrM' except
-- that the list is generated lazily.
unfoldrM :: MonadUnliftIO m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f z = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u z)
  where
    go !u !b = do
      m <- unliftIO u (f b)
      case m of
        Nothing      -> pure []
        Just (!a, b') -> do
          rest <- IO.unsafeInterleaveIO (go u b')
          pure (a:rest)

-- | Traverses the function over the list and produces a lazy list in a
-- monadic context.
--
-- It is intended to be like the "standard" 'traverse' except
-- that the list is generated lazily.
traverseM :: MonadUnliftIO m => (a -> m b) -> [a] -> m [b]
traverseM f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u as)
  where
    go _ [] = pure []
    go !u (v:vs) = do
      !res <- unliftIO u (f v)
      rest <- IO.unsafeInterleaveIO (go u vs)
      pure (res:rest)

forM :: MonadUnliftIO m => [a] -> (a -> m b) -> m [b]
forM = flip traverseM

forceM :: (Monad m, NFData a) => m a -> m a
forceM = (force <$!>)

-- Internal
sequenceIO :: [IO a] -> IO [a]
sequenceIO = IO.unsafeInterleaveIO . go
  where go :: [IO a] -> IO [a]
        go []       = return []
        go (fa:fas) = (:) <$> fa <*> IO.unsafeInterleaveIO (go fas)
