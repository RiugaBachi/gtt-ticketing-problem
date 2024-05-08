{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ticketing
import System.Random
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Maybe
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Criterion.Types

genSeatingSlices :: (Int, Int) -> Int -> Int -> Int -> Int -> IO [SeatingSlice]
genSeatingSlices costRange unavailFrac secs rows seats = join <$>
  [1..secs] `forM` \sec -> join <$>
    [1..rows] `forM` \row -> catMaybes <$>
      [2..(seats * 2)] `forM` \seat -> do
        cost <- randomRIO costRange
        unavailable <- (== unavailFrac) <$> randomRIO (1, seats)
        if unavailable then pure Nothing else
          pure $ Just $ SeatingSlice "test" (T.pack $ show secs) row seat seat (realToFrac cost) Even

main :: IO ()
main =
  defaultMain
    [ bgroup "AT&T Stadium (200 sections x (20 rows x 25 seats))" 
        [ bench "randomized (96% available, select 7, 210.32 budget)" $
            perRunEnv (genSeatingSlices (10, 60) 25 200 20 25) $ \slices ->
              pure $ findOptimalSeatingCombinations 7 210.32 slices
        , bench "randomized (80% available, select 3, 80.64 budget)" $
            perRunEnv (genSeatingSlices (10, 60) 5 200 20 25) $ \slices ->
              pure $ findOptimalSeatingCombinations 3 80.64 slices
        , bench "randomized (99% available, select 2, unlimited budget)" $
            perRunEnv (genSeatingSlices (10, 60) 100 200 20 25) $ \slices ->
              pure $ findOptimalSeatingCombinations 2 999 slices
        ]
    , bgroup "Yokohama Arena (34 sections x (20 rows x 25 seats))" 
        [ bench "randomized (99% available, select 2, 30 budget)" $
            perRunEnv (genSeatingSlices (10, 60) 100 34 20 25) $ \slices ->
              pure $ findOptimalSeatingCombinations 2 30 slices
        , bench "randomized (99% available, select 1, unlimited budget)" $
            perRunEnv (genSeatingSlices (10, 60) 100 34 20 25) $ \slices ->
              pure $ findOptimalSeatingCombinations 1 999 slices
        ]
    ]
