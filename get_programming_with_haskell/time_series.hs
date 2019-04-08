{-
Capstone project for Unit 3 of 'Get Programming with Haskell'

Which focuses on Product and Sum Types, Parameterized Types,
Semigroups/Monoids/Maybe as useful examples of type classes
and practical parameterized types.

-}

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- Sample data: need to model it in a way that can be "made sense" of

file1 :: [(Int,Double)]
file1 = [ (1,200.1), (2,199.5), (3,199.4)
        , (4,198.9), (5,199.0), (6,200.2)
        , (9,200.3),(10,201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11,201.6), (12,201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2),(11, 201.6),(12, 201.5)
        ,(13, 201.5),(14, 203.5),(17, 210.5)
        ,(24, 215.1),(25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27,220.5),(28,223.8)
        ,(29, 222.8), (30, 223.8),(31, 221.7)
        ,(32, 222.3), (33, 220.8),(34, 219.4)
        ,(35, 220.1),(36, 220.6)]

-- Given a point in time, we may or may not have financial data for it
data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
  where completeTimes  = [minimum times .. maximum times] -- get all possible types
        timeValueMap   = Map.fromList $ zip times values  -- create a dictionary of times/values
        extendedValues = map
                         (\v -> Map.lookup v timeValueMap)
                         completeTimes                    -- use map's lookup to find the missing values, too

-- Turn file data into TS instances
fileToTS :: [(Int, a)] -> TS a
fileToTS tvPairs = createTS times values
  where (times, values) = unzip tvPairs

-- helper function to be able to `show` a TS:
showTVPair :: Show a => Int -> (Maybe a) -> String
-- notice that `mconcat` is a function in the Monoid typeclass that
-- is defined as mconcat = foldr mappend mempty; where `mempty` is
-- the identity function for the given type, and mappend is the `<>`
-- operator, as in Semigroup: it combines two instances of a type into
-- a third. For Strings, mappend creates a new string separated by ""
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

-- now we can make TS an instance of show, specifying that
-- a _must_ be a show-able type, too
instance Show a => Show (TS a) where
  show (TS times values) = mconcat rows
    where rows = zipWith showTVPair times values

{-
At this point, one can run fileToTS and see:
λ> fileToTS file1
1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2
7|NA
8|NA
9|200.3
10|201.2
11|NA
12|202.9


-}

ts1, ts2, ts3, ts4 :: TS Double
ts1 = fileToTS file1
ts2 = fileToTS file2
ts3 = fileToTS file3
ts4 = fileToTS file4

{- Goal 1: stitch TS files together
   ================================

   Since data for the same time may occur in different files, we'll
   give priority to the newest one. This lends itself very well to Map.Map
-}

-- helper function for inserting (k, Maybe v) pairs into a (k, v) map,
-- to be able to make TS an instance of Semigroup.
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (key, (Just value)) = Map.insert key value myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
  where bothTimes = mconcat [t1, t2]
        completeTimes = [minimum bothTimes .. maximum bothTimes]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2) -- update the initial map with values from v2, removes dupes
        combinedValues = map (\v -> Map.lookup v updatedMap)
                         completeTimes

-- now we're ready to make TS a Semigroup instance:
-- we know how to combine individual TS values
instance Semigroup (TS a) where
  (<>) = combineTS


{-
With the work done above, we now are able to zip together different timelines,
fill in the blanks and determine precedence of newer entries:
λ> ts1 <> ts2
1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2
7|NA
8|NA
9|200.3
10|201.2
11|201.6
12|201.5
13|201.5
14|203.5
15|204.9
16|207.1
17|NA
18|210.5
19|NA
20|208.8

-}

-- Next, you want to be able to concat (combine) a list of many TS files. This is mconcat,
-- which you get for free in a Monoid instance. Having <> defined (mappend), all you need is
-- identity (mempty):

instance Monoid (TS a) where
  mempty  = TS [] []
  mappend = (<>)

-- now you can do things like mconcat [ts1, ts2, ts3]

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

-- Goal 2: do some analytics!

mean :: (Real a) => [a] -> Double
mean xs = total/count
  where total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing -- no mean for a TS with empty values
meanTS (TS times values) = if all (== Nothing) values
                           then Nothing
                           else Just avg
  where justVals = filter isJust values -- isJust, a predicate, comes with Maybe
        cleanVals = map fromJust justVals
        avg = mean cleanVals

{-
λ> meanTS tsAll
Just 210.5966666666667
-}

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

-- helper function to transform any "simple" comparator
-- into one that can apply to TS entries
makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare func = newFunc
  where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        newFunc (i1, Just val1) (i2, Just val2) =
          if func val1 val2 == val1
          then (i1, Just val1)
          else (i2, Just val2)

{-

with the above function, you can now make the built-in `max` work with TS values:

λ> makeTSCompare max (3, Just 200) (4, Just 10)
(3,Just 200)

And now, we can apply any comparison function to TS values, as a next logical step
-}

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
  where pairs = zip times values
        best  = foldl (makeTSCompare func) (0, Nothing) pairs

minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min -- note the liberal reliance on partial application: this returns a fn that has the signature declared,
                      -- which is a _part_ of the signature of compareTS, with the func (a -> a -> a) partially applied.

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max


-- Goal 3: further analysis: diffs, smoothing data

-- implement `diff` to be able to see the change between any two data points
-- (either of which could be an `NA` value). It only makes sense to operate
-- on Num[eric] instances of TS
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues) -- Nothing is the zeroth point in a diff series: start from nothing
  where shiftValues = tail values -- values shifted to the right: compare each with the preceding
        diffValues  = zipWith diffPair shiftValues values

{-

We can now see how much change it took to reach each point in the series
(note that the first one is NA, since nothing changed from Nothing to the first value)

λ> diffTS tsAll
1|NA
2|-0.5999999999999943
3|-9.999999999999432e-2
4|-0.5
5|9.999999999999432e-2

1|200.1
2|199.5
3|199.4
4|198.9
5|199.0
6|200.2

And we can now the mean change, too:

λ> meanTS $ diffTS tsAll
Just 0.6076923076923071

Which could be interpreted as "for each known data point, the data has incremented 60%"

The next thing is a moving average as an approach to smoothing:

-}

meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (== Nothing) vals
                 then Nothing
                 else (Just avg)
  where avg = mean $ map fromJust vals

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n = if length nextVals == n
                   then meanMaybe nextVals:movingAvg restVals n -- all the moving averages for each n-element chunk
                   else []
  where nextVals = take n vals
        restVals = tail vals

movingAverageTS :: (Real a) => TS a -> Int -> TS Double
movingAverageTS (TS [] []) n = TS [] []
movingAverageTS (TS times values) n = TS times smoothedValues
  where ma = movingAvg values n
        nothings = replicate (n `div` 2) Nothing -- create nothings to pad the averages
        smoothedValues = mconcat [nothings, ma, nothings]

