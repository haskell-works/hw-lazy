{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Control.Monad.Lazy
  ( replicateM
  , sequenceM
  , unfoldrM
  , traverseM
  , traverseStateM
  , forM
  , forceM
  ) where

import Control.DeepSeq (force, NFData)
import Control.Monad ((<$!>))
import Control.Monad.IO.Unlift (askUnliftIO, MonadIO(..), MonadUnliftIO, UnliftIO(..))

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
traverseM :: forall m a b. MonadUnliftIO m => (a -> m b) -> [a] -> m [b]
traverseM f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u as)
  where
    go :: UnliftIO m -> [a] -> IO [b]
    go _ [] = pure []
    go !u (v:vs) = do
      !res <- unliftIO u (f v)
      rest <- IO.unsafeInterleaveIO (go u vs)
      pure (res:rest)

traverseStateM :: forall m s a b. MonadUnliftIO m => s -> (s -> a -> m (s, b)) -> [a] -> m [b]
traverseStateM s f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go s u as)
  where
    go :: s -> UnliftIO m -> [a] -> IO [b]
    go _ _ [] = pure []
    go t !u (v:vs) = do
      (t', !res) <- unliftIO u (f t v)
      rest <- IO.unsafeInterleaveIO (go t' u vs)
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
