module Data.PackedSet where

import qualified Data.Set   as ST
import qualified Data.Map   as MP


data PackedSet a = PackedSet
        -- Total number of elements, including interval elements.
    { psSize            :: Int
        -- Primary interval ends.
    , psPrimaryIv       :: (a,a)
        -- Lower end of all the secondary intervals; for speedy lookups.
    , psSecondaryIvsLE  :: ST.Set a
        -- Secondary intervals; we assume at some point the main interval
        -- will swallow up all these secondary intervals.
    , psSecondaryIvs    :: MP.Map a (a,a)
        -- Outliers are not part of either the main interval or the secondary
        -- intervals, so we keep these elements in a separate set. Eventually,
        -- these will merge in one of the intervals.
    , psOutliers        :: ST.Set a
    } deriving (Show)


-- | How often should we pack-up a set
packingThrottle :: Int
packingThrottle = 100

-- Main differences between Data.PackedSet and Data.Set:
--
-- Type constraints on set members: Ord, Num.
-- Lack of a 'delete' operation.

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty packet set.
empty :: PackedSet Int
empty =
    PackedSet { psSize              = 0
              , psPrimaryIv         = (0,0)
              , psSecondaryIvsLE    = ST.empty
              , psSecondaryIvs      = MP.empty
              , psOutliers          = ST.empty
              }


-- | /O(1)/. Create a singleton packet set.
singleton :: (Ord a, Num a) => a -> PackedSet a
singleton x =
    PackedSet { psSize              = 1
              , psPrimaryIv         = (x,x)
              , psSecondaryIvsLE    = ST.empty
              , psSecondaryIvs      = MP.empty
              , psOutliers          = ST.empty
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
member :: (Ord a, Num a) => a -> PackedSet a -> Bool
member m p =
    if (psSize p == 0)
        then False
        else if (_belongsTo m mi) || (ST.member m ol)
                    then True
                    else _intervalsLookup m p
    where
        ol = psOutliers p
        mi = psPrimaryIv p

-- | /O(log n)/. Is the element not in the packed set?
notMember :: (Ord a, Num a) => a -> PackedSet a -> Bool
notMember m p = not $ member m p


{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in a packed set if it's not a member already.
insert :: (Ord a, Num a) => a -> PackedSet a -> PackedSet a
insert m p =
    if (psSize p) == 0
        -- If the set is empty, create a singleton.
        then singleton m
        else if (_belongsTo m $ psPrimaryIv p)
            -- If the element is already in the main interval, nothing to do.
            then p
            -- Check if this element can expand the main interval.
            else if expandedMainIv == True
                    -- Pack-up the set before returning it
                    then postMainExpandPack p newSize newMainIv
                    else p
                        -- if (expandedSecondaryIv == True)
                            -- then
                            -- else
    where
        (psll, psul) = psPrimaryIv p
        (expandedMainIv, newMainIv) = _maybeExpand m $ psPrimaryIv p
        newSize = (psSize p) + 1
        -- (expandedSecondaryIv) = _maybeExpandSecondaryIv
        -- packs the set after the main interval was expanded
        postMainExpandPack p size mainIv =
            p { psSize = size, psPrimaryIv = mainIv}


{--------------------------------------------------------------------
  Private functions
--------------------------------------------------------------------}
-- Looks-up an element in all the outlying intervals of a packed set.
_intervalsLookup :: (Ord a, Num a) => a -> PackedSet a -> Bool
_intervalsLookup m p =
    maybe False
          (\se -> maybe False validateInterval $ mInterval se)
          mIntervalSE
    where
        -- find the interval lower end, if any
        mIntervalSE = ST.lookupLE m (psSecondaryIvsLE p)
        mInterval k = MP.lookup k (psSecondaryIvs p)
        validateInterval (_, iGE) = iGE > m


-- Returns true if an element is inside an interval.
_belongsTo :: (Ord a, Num a) => a -> (a,a) -> Bool
_belongsTo m (lowerLim, upperLim) =
    m >= lowerLim && m <= upperLim


-- Tries to expand a given interval with an element, either to the left or to
-- the right, wherever is possible.
_maybeExpand :: (Ord a, Num a) => a -> (a,a) -> (Bool, (a,a))
_maybeExpand m (lowerLim, upperLim) =
    if (m == lowerLim - 1)
        then (True, (m,upperLim))
        else if (m == upperLim + 1)
            then (True, (lowerLim, m))
            else (False, (lowerLim, upperLim))