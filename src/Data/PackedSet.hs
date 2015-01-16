module Data.PackedSet where


import qualified Data.Set   as S


data PackedSet a = PackedSet
    { psSize        :: Int
    , psLowerLimit  :: a
    , upperLimit    :: a
    , outliers      :: S.Set (a)
    }


-- instance Show PackedSet where
--     show PackedSet {upperLimit = u, outliers = o} =
--         "DG: {" ++ show u ++ ", " ++ show (S.size o) ++ "}"

