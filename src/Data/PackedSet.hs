module Data.PackedSet where

import Data.Maybe           (maybe)
import qualified Data.Set   as ST
import qualified Data.Map   as MP


data PackedSet a = PackedSet
       -- Total number of elements.
    { psSize            :: Int
        -- Main interval limits.
    , psMainInterval    :: (a,a)
        -- Lower end of the intervals, speeds-up lookups.
    , psIntervalsLE     :: ST.Set a
        -- All the outlying interval; we assume at some point the main interval
        -- will swallow up all these.
    , psIntervals       :: MP.Map a (a,a)
        -- Outliers are not part of either the main interval or the other
        -- intervals, so keep them in a separate set. We assume eventually
        -- these will merge in
    , psOutliers        :: ST.Set a
    } deriving (Show)


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty packet set.
empty :: PackedSet Int
empty =
    PackedSet { psSize          = 0
              , psMainInterval  = (0,0)
              , psIntervalsLE   = ST.empty
              , psIntervals     = MP.empty
              , psOutliers      = ST.empty
              }


-- | /O(1)/. Create a singleton packet set.
singleton :: Ord a => a -> PackedSet a
singleton x =
    PackedSet { psSize          = 1
              , psMainInterval  = (x,x)
              , psIntervalsLE   = ST.empty
              , psIntervals     = MP.empty
              , psOutliers      = ST.empty
              }


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is this packed set empty?
null :: PackedSet a -> Bool
null PackedSet { psSize = s } =
    s == 0


-- | /O(1)/. Returns the number of elements in the packed set.
size :: PackedSet a -> Int
size PackedSet {psSize = s} =
    s


-- | /O(log n)/. Lookup a member in the packed set.
-- TODO!
member :: Ord a => a -> PackedSet a -> Bool
member m p =
    if (psSize p == 0)
        then False
        else if (_belongsTo m mi) || (ST.member m ol)
                    then True
                    else _intervalsLookup m p
    where
        ol = psOutliers p
        mi = psMainInterval p

-- | /O(log n)/. Is the element not in the packed set?
notMember :: Ord a => a -> PackedSet a -> Bool
notMember m p = not $ member m p


{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in a packed set if it's not a member already.
-- insert :: Ord a => a -> PackedSet a -> PackedSet a
-- insert m p =
--     if (psSize p) == 0
--         then singleton m
--         else if condition
--             then expression
--             else expression
--             -- if (m == psll - 1)
--             --     then newPSULimit
--             --     else if (m == psll - 1)
--             --         then expression
--             --         else expression
--     where
--         (psll, psul) = psMainInterval p
--     --     pso = psOutliers p
--         newPSULimit = p { psMainInterval = (m, psul) }




{--------------------------------------------------------------------
  Private functions
--------------------------------------------------------------------}
-- Looks-up an element in all the outlying intervals of a packed set.
_intervalsLookup :: Ord a => a -> PackedSet a -> Bool
_intervalsLookup m p =
    maybe False
          (\se -> maybe False validateInterval $ mInterval se)
          mIntervalSE
    where
        -- find the interval lower end, if any
        mIntervalSE = ST.lookupLE m (psIntervalsLE p)
        mInterval k = MP.lookup k (psIntervals p)
        validateInterval (_, iGE) = iGE > m


-- Returns true if an element is inside an interval.
_belongsTo :: Ord a => a -> (a,a) -> Bool
_belongsTo m (lowerLim, upperLim) =
    m >= lowerLim && m <= upperLim