module Maid.Helpers
( enumerate
, takeWhile'
)
where

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

takeWhile' :: (a -> Bool) -> [a] -> ([a], [a], Integer)
takeWhile' predicate (h:t) | predicate h =
    let (left, right, left_size) = takeWhile' predicate t in
        (h : left, right, left_size + 1)
takeWhile' _ xs = ([], xs, 0)
