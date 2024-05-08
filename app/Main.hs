{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Ticketing
import Control.Monad
import Data.Function
import Data.List (sortOn, groupBy, find, nub, nubBy, sort, (\\))
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy qualified as B
import Data.Csv qualified as Csv
import Data.Vector qualified as V
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Text (Text)

-- | Hastily scrapped together but it'll do
visualizeSeating :: [Seat] -> IO ()
visualizeSeating seats = do
  let rows = sort $ nub $ seatRow <$> seats
  let begin = seatNumber (head $ sortOn seatNumber seats)
  let end = seatNumber (last $ sortOn seatNumber seats)
  forM_ rows $ \r -> do
    let seats' = filter (\s -> seatRow s == r) seats
    forM_ [x | x <- [begin..end], (x - begin) `rem` 2 == 0] $ \n -> do
      maybe (putStr ". ") (const $ putStr "o ") $ find (\s -> seatNumber s == n) seats'
    putStr "\n"

main :: IO ()
main = do
  dataFile <- B.readFile "data.csv"
  let (_, slices) = either (error "CSV data malformed") id $ Csv.decodeByName @SeatingSlice dataFile 
  let seatCombinations = findOptimalSeatingCombinations 3 210.32 $ V.toList slices
  forM_ (zip [(0 :: Int)..] seatCombinations) $ \(ix, seats) -> do
    putStrLn $ "Combination " <> show ix <> "."
    putStrLn $ "Total price: " <> show (sum $ seatPrice <$> seats)
    forM_ seats $ putStrLn . show
    visualizeSeating seats
