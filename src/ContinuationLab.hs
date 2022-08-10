module ContinuationLab where

-- This is the part where Lynn tries to understand shift/reset

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont

step :: Int -> ContT [Int] IO Int
step n = do
  -- liftIO $ putStrLn ("before shift: step " ++ show n)
  shiftT $ \k -> do
    -- liftIO $ putStrLn ("step " ++ show n)
    lift $ do
      v1 <- k n
      v2 <- k (-n)
      print (v1, v2)
      pure (0 : v1 ++ v2)

-- lift $ (n:) <$> k (n^2)
-- lift $ (-n:) <$> k (-n^2)

example :: ContT [Int] IO [Int]
example = do
  v <- resetT $ do
    liftIO $ putStrLn "begun reset"
    r <- mapM step [1 .. 3]
    -- liftIO $ putStrLn "the end!"
    liftIO $ print r
    pure r
  liftIO $ putStrLn $ "post-reset: " ++ show v
  pure v

runExample :: IO [Int]
runExample = evalContT example
