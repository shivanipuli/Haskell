module Interval where

---- Intervals ----

data Interval a = Range a a

isEmptyInterval :: Ord a => Interval a -> Bool
isEmptyInterval (Range start end) = start >= end

instance Ord a => Eq (Interval a) where
  i1@(Range start1 end1) == i2@(Range start2 end2) =
    (isEmptyInterval i1 && isEmptyInterval i2) ||
    (start1 == start2 && end1 == end2)

-- Intervals are ordered by starting time. This is necessary for normalizeIS.
instance Ord a => Ord (Interval a) where
  Range start1 end1 <= Range start2 end2 = (start1, end1) <= (start2, end2)

instance (Show a, Ord a, Bounded a) => Show (Interval a) where
  show i@(Range start end)
    | isEmptyInterval i = "Empty"
    | start == minBound && end == maxBound = "All"
    | start == minBound = "<" ++ show end
    | end == maxBound = ">=" ++ show start
    | otherwise = show start ++ "<=_<" ++ show end

instance (Read a, Ord a, Bounded a) => Read (Interval a) where
  -- Read is a little less straightforward because it uses
  -- parsers, which we'll learn about later. Just replace the
  -- undefineds below with what you want to return and
  -- everything will work.
  -- hint: use the function `read` to get the value of an
  -- endpoint from `next`.
  readsPrec _ "Empty"        = [(Range minBound minBound, "")] --coded with Ben in class
  readsPrec _ "All"          = [(Range minBound maxBound, "")]
  readsPrec _ ('>':'=':next) = [(Range (read next) maxBound, "")]
  readsPrec _ ('<':next)     = [(Range minBound (read next), "")]
  readsPrec _ str =
    -- Don't worry about this case. It is a
    -- bit clunky. We will learn a better
    -- option later in the course.
    case reads str of
      (start, '<':'=':'_':'<':rest):_ ->
        case reads rest of
          [] -> error "error parsing interval"
          (end,_):_ -> [(Range start end, "")]
      _ -> error "error parsing interval"

intersectIntervals :: Ord a => Interval a -> Interval a -> Interval a
intersectIntervals (Range x y) (Range a b) = Range (max x a) (min y b)

---- Interval Sets ----

-- An interval set might have intervals that overlap or touch. Don't worry about
-- simplifying these cases in the following functions. That is handled just
-- before displaying by normalizeIS.

type IntervalSet a = [Interval a]

toIS :: Interval a -> IntervalSet a
toIS = (:[])

emptyIS :: IntervalSet a
emptyIS = []

allIS :: Bounded a => IntervalSet a 
allIS = [Range minBound maxBound]

removeEmptyIntervals :: Ord a => IntervalSet a -> IntervalSet a -- coded in class with Ben
removeEmptyIntervals = filter $ not . isEmptyInterval

intersectISI :: Ord a => IntervalSet a -> Interval a -> IntervalSet a --Coded in class with Ben
-- intersectISI iset interval = removeEmptyIntervals (map (\x -> intersectIntervals x interval) iset)
intersectISI iset interval = removeEmptyIntervals ( foldr (\x y -> toIS (intersectIntervals x interval) ++ y) emptyIS iset )

-- The complement of an interval must return an interval set because it may
-- result in two disjoint intervals.
complementInterval :: (Bounded a, Ord a) => Interval a -> IntervalSet a
complementInterval (Range min max)
  | min == minBound = removeEmptyIntervals [Range max maxBound]
  | otherwise       = removeEmptyIntervals [Range minBound min, Range max maxBound] 

-- An interval minus an interval must return an interval set because the second
-- could cut a hold in the middle of the first.
-- Big Hint: Use complements and intersections.
--
-- IMPORTANT NOTE: There cannot be any empty intervals left over in the ouptut
-- of this function. Leaving them does not affect the results, but it may make
-- your program too slow! You are welcome to use removeEmptyIntervals for this.
differenceIntervals :: (Ord a, Bounded a) => Interval a -> Interval a -> IntervalSet a
differenceIntervals int1 int2 = removeEmptyIntervals (intersectISI (complementInterval int2) int1)


-- interval set minus an interval
differenceISI :: (Ord a, Bounded a) => IntervalSet a -> Interval a -> IntervalSet a
differenceISI iset interval = removeEmptyIntervals (concatMap (\x -> differenceIntervals x interval) iset)


---- Helpers for interval sets ----

intersection :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> IntervalSet a
intersection iset1 = concatMap (intersectISI iset1)
--intersection iset1 iset2 = intersectISI (foldr intersectISI allIS iset2 iset1) iset1


union :: IntervalSet a -> IntervalSet a -> IntervalSet a
union = (++)


-- | Removes iset2 from iset1
difference :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> IntervalSet a
difference = foldr $ flip differenceISI   -- Use foldr.


---- Queries on interval sets ----
 -- Worked with Omar and Emi on intersectAll and differenceAll
intersectAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
intersectAll = foldr intersection allIS -- Use foldr.
--foldr1 intersection => doesn't need base case. base case is first element

unionAll :: [IntervalSet a] -> IntervalSet a
unionAll = concat 

-- Subtract from the first interval set all the remaining interval sets.
differenceAll :: (Ord a, Bounded a) => [IntervalSet a] -> IntervalSet a
differenceAll [] = emptyIS
differenceAll (first:rest) = foldr (flip difference) first rest -- Use foldr

---- Boolean Helpers ----

isEmpty :: Ord a => IntervalSet a -> Bool
isEmpty = null . removeEmptyIntervals

-- Hint: areDisjoint and isSubset are simpler than areEqual. Use what you have
-- already defined.

-- two interval sets are disjoint if they do not overlap
areDisjoint :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
areDisjoint iset1 iset2 = isEmpty $ intersection iset1 iset2 --differenceAll [iset1,iset2] == iset1

isSubset :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
isSubset is1 is2 = isEmpty (difference is1 is2) -- is1 == intersection is1 is2

areEqual :: (Ord a, Bounded a) => IntervalSet a -> IntervalSet a -> Bool
areEqual is1 is2 = is1 `isSubset` is2 && is2 `isSubset` is1


---- Boolean Queries ----
-- worked with Emi for pseudocode 
areAllDisjoint :: (Ord a, Bounded a) => [IntervalSet a] -> Bool
areAllDisjoint [] = True
areAllDisjoint (first:rest) = all ( areDisjoint first ) rest && areAllDisjoint rest -- Hint : this function is recursive.

areAllEqual :: (Ord a, Bounded a) => [IntervalSet a] -> Bool
areAllEqual [] = True
areAllEqual (first:rest) = all ( areEqual first ) rest

