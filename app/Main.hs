{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.Generics
import Debug.Trace
import Control.Arrow ((>>>))
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Functor ((<&>))
import Data.Function
import Data.List (sortOn, groupBy, find, nub, nubBy, sort, (\\))
import Data.Bifunctor (first, second)
import Data.ByteString.Lazy qualified as B
import Data.Csv qualified as Csv
import Data.Vector qualified as V
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as M
import Data.Text (Text)

-- | Didn't feel like adding Relude for this
dup :: a -> (a, a)
dup x = (x, x)

-- | Utility for chaining predicates
(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) = liftA2 (&&)

data Seat = Seat
  { seatRow :: Int
  , seatSection :: Text
  , seatNumber :: Int
  , seatPrice :: Float
  } deriving (Show, Eq, Ord)

isSameSeat :: Seat -> Seat -> Bool
isSameSeat s1 s2 =
  seatRow s1 == seatRow s2 && 
  seatNumber s1 == seatNumber s2 && 
  seatSection s1 == seatSection s2

data Consecutivity 
  = Odd 
  | Even
  deriving (Show)

instance Csv.FromField Consecutivity where
  parseField "odd" = pure Odd
  parseField "even" = pure Even
  parseField _ = mzero

data SeatingSlice = SeatingSlice
  { event :: Text
  , section :: Text
  , row :: Int
  , startSeat :: Int
  , endSeat :: Int
  , cost :: Float
  , consecutivity :: Consecutivity 
  } deriving (Show, Generic, Csv.FromNamedRecord)


expandSliceIntoSeats :: SeatingSlice -> [Seat]
expandSliceIntoSeats SeatingSlice {..} =
  [ x | x <- [startSeat..endSeat], (x `rem` 2) `check` 0] <&> \i ->
    Seat { seatRow = row
         , seatSection = section
         , seatNumber = i
         , seatPrice = cost
         }
  where
    check = case consecutivity of { Even -> (==); Odd -> (/=); }

subslicesOf :: Int -> [a] -> [[a]]
subslicesOf n xs
  | length xs >= n = take n xs : subslicesOf n (drop 1 xs)
  | otherwise = []

-- | Given a @partySize@, @budget@, and a set of @availableSeats@, 
--   find the most optimal set of single-row adjacent seats or piggyback seats.
--
--   Preference will be given to adjacent seats. If those cannot be found,
--   then piggyback seats will be searched for instead.
--
--   Furthermore, seats near the ends of a section (closest to seat number 0)
--   will be prioritized when available. This takes precedence over optimizing for
--   lowest cost.
--
--   Returns @Nothing@ if no seats within the requisite criteria can be found.
findOptimalSeating 
  :: Int
  -- ^ Size of the party
  -> Float
  -- ^ Combined budget
  -> [SeatingSlice]
  -- ^ Available seats
  -> Maybe [Seat]
findOptimalSeating partySize budget availableSeats =

  let 
    -- Step 1. Preprocess the data into a more intuitive format
    seatsBySection :: Map Text [Seat] = 
      fmap (nubBy isSameSeat) $ M.fromListWith (<>) $ flip fmap availableSeats $
        dup >>> first section
            >>> second expandSliceIntoSeats
    -- Step 2. Cull the dataset
    -- We can cull entire sections based on several criteria that are computationally cheap to verify
    culledSeatsBySection :: Map Text [Seat] = 
      cull seatsBySection
    -- Step 3. (Condition 1) Find same-row adjacent seats
    -- We start with this as it is computationally less expensive than row-scanning for piggyback seats,
    -- and it is generally preferable over piggyback seats in most real world scenarios (ref. Assumption 1)
    adjacentSeatsBySection :: Map Text (Maybe [Seat]) = 
      findAdjacentSeats <$> culledSeatsBySection
    -- Step 4. (Condition 2) Find piggyback seats
    -- Now we can perform the comparatively more expensive double-row scanning checks; again, on the culled dataset.
    piggybackSeatsBySection :: Map Text (Maybe [Seat]) = 
      findPiggybackSeats <$> culledSeatsBySection
    -- Step 5. Select the first set of seats between the combined solutions of Step 3 and Step 4
    -- By virtue of list laziness, piggyback seats shouldn't be computed unless no seats from Step 3 exist.
    optimalSeats :: Maybe [Seat] = 
      listToMaybe $ catMaybes $ M.elems adjacentSeatsBySection <> M.elems piggybackSeatsBySection
  in
    optimalSeats
  where
    -- | Culls entire sections based on eligibility criteria; namely:
    --    1. There exist enough available seats within the section to begin with
    --    2. The cheapest @partySize@ number of seats that are available can fit within budget
    --
    --  As we profile for hotspots, we can think of potentially other cheap criteria to weed out
    --  entire sections as they become a problem. This is a good start though.
    cull :: Map Text [Seat] -> Map Text [Seat]
    cull = M.filter (hasSufficientSeats .&&. canFitWithinBudget)

    -- | Find the first set of single-row adjacent seats within criteria
    findAdjacentSeats :: [Seat] -> Maybe [Seat]
    findAdjacentSeats seats =
      let
        seatsByRows = partitionRows seats
        subslicesByRows = subslicesOf partySize <$> seatsByRows
        subslicesWithinCriteria = 
          find (isCorrectSizeForParty .&&. canFitWithinBudget .&&. hasNoDiscontinuities) <$> subslicesByRows
      in
        msum subslicesWithinCriteria

    -- | Find a set of seats on two adjacent rows that:
    --     1. Is within budget
    --     2. Can accomodate the party size
    --     3. Falls within the criteria of what is considered a "piggyback shape"
    findPiggybackSeats :: [Seat] -> Maybe [Seat]
    findPiggybackSeats seats =
      let
        seatsByRows = partitionRows seats
        tuplesOfAdjacentRows :: [([Seat], [Seat])] = 
          zip seatsByRows (drop 1 seatsByRows)
        tuplesWithinCriteria = 
          findPiggybackSeatsInRowPair <$> tuplesOfAdjacentRows
      in
        listToMaybe $ catMaybes tuplesWithinCriteria
      where
        -- The concept behind this is simple:
        --   1. We want to sort the combined seats of both rows by @seatNumber@
        --   2. We generate a list of subslices of the aforementioned list
        --   3. We iterate through the subslices to find the first that matches our criteria
        --
        -- The motivation is that we ideally want seats closer to the ends (for the purposes of this demo,
        -- the seats closest to seat number 0); by sorting on seat number, the subslices will gradually
        -- 'snake' from one end to the other, thereby making the the first @Just@ solution the most ideal.
        findPiggybackSeatsInRowPair :: ([Seat], [Seat]) -> Maybe [Seat]
        findPiggybackSeatsInRowPair (row1,  row2) =
          let
            sortedBySeatNumber = sortOn seatNumber $ row1 <> row2
            -- Note: 'subslicesOf' does not guarantee a slice of size N if the input list is too small;
            -- we still need to verify via the 'isCorrectSizeForParty' predicate.
            subslices = subslicesOf partySize sortedBySeatNumber
          in
            find (isCorrectSizeForParty .&&. canFitWithinBudget .&&. isPiggybackShape) subslices
          
        -- | A set of seats is considered to have a "piggyback shape" when:
        --     1. They fall on exactly two rows
        --     2. The two rows are directly adjacent (this is currently not checked internal to the function)
        --           TODO: consider adding a guard clause for point #2
        --     3. The majority of seats are directly across from each other*
        --     4. There are no discontinuities in either row of seats
        isPiggybackShape :: [Seat] -> Bool
        isPiggybackShape (partitionRows -> [row1, row2]) =
          hasNoDiscontinuities row1 && hasNoDiscontinuities row2 && isMajorityDirectlyAcross (row1, row2)
          where
            -- | A majority of seats is defined as half of the @partySize@, rounded up.
            --   * EXCEPTION: 
            --       When one of the rows has no unique seat numbers compared to the other row,
            --       we still consider the two rows to be majority directly across.
            isMajorityDirectlyAcross :: ([Seat], [Seat]) -> Bool 
            isMajorityDirectlyAcross (row1, row2) = 
              let
                row1SeatNumbers = seatNumber <$> row1
                row2SeatNumbers = seatNumber <$> row2
                row1Uniques = length $ row1SeatNumbers \\ row2SeatNumbers
                row2Uniques = length $ row2SeatNumbers \\ row1SeatNumbers
                majorityOfSeats = ceiling @Float $ realToFrac partySize / 2
              in
                any (== 0) [row1Uniques, row2Uniques] || row1Uniques + row2Uniques < majorityOfSeats
        isPiggybackShape _ = False -- If the seats can't divide into two rows, they aren't piggybacks

    -- | Groups a list of seats by their rows
    partitionRows :: [Seat] -> [[Seat]]
    partitionRows = groupBy ((==) `on` seatRow) . sortOn seatRow 

    -- | Generate all missing seat numbers in a list of seats, assuming the minimum and maximum
    --   seat numbers in the list bound the range.
    allMissingSeatNumbers :: [Seat] -> [Int]
    allMissingSeatNumbers availableSeats =
      let 
        availableSeatNumbers = seatNumber <$> availableSeats
      in 
        [ x 
        | x <- allSeatNumbersInRange (minimum availableSeatNumbers) (maximum availableSeatNumbers)
        , not (x `elem` availableSeatNumbers) 
        ]

    -- | Generate all valid seat numbers within a given range (i.e. every other number)
    allSeatNumbersInRange :: Int -> Int -> [Int]
    allSeatNumbersInRange begin end = 
      [ x | x <- [begin..end] , (x - begin) `rem` 2 == 0 ]
    
    -- | A discontinuity is defined as a gap of one or more missing seats between 
    --   one seat in a row and the next seat within that same row.
    --
    --   Since the list of seats per row only contains available seats,
    --   a discontinuity is simply the presence of a missing seat number.
    hasNoDiscontinuities :: [Seat] -> Bool 
    hasNoDiscontinuities row =
      length (allMissingSeatNumbers row) == 0

    -- | Does this set have enough seats to potentially accompany the party?
    hasSufficientSeats :: [Seat] -> Bool
    hasSufficientSeats seats = length seats >= partySize

    -- | Does this set of seats have the same size as the party?
    isCorrectSizeForParty :: [Seat] -> Bool
    isCorrectSizeForParty seats = length seats == partySize

    -- | Can the cheapest N seats from this set (best case) stay within budget?
    canFitWithinBudget :: [Seat] -> Bool
    canFitWithinBudget seats = sum (seatPrice <$> take partySize (sortOn seatPrice seats)) < budget

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
  let seats = maybe [] id $ findOptimalSeating 7 210.32 $ V.toList slices
  putStrLn $ "Total price: " <> show (sum $ seatPrice <$> seats)
  forM_ seats $ putStrLn . show
  visualizeSeating seats
