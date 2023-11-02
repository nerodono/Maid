module Maid.Helpers
( enumerate
, takeWhile'
, rightToMaybe
, maybeToEither
)
where

enumerate :: [a] -> [(Integer, a)]
enumerate = zip [0..]

takeWhile' :: (a -> Bool) -> [a] -> ([a], [a], Integer)
takeWhile' predicate (h:t) | predicate h =
    let (left, right, left_size) = takeWhile' predicate t in
        (h : left, right, left_size + 1)
takeWhile' _ xs = ([], xs, 0)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right r) = Just r
rightToMaybe (Left _) = Nothing

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither left Nothing = Left left
maybeToEither _ (Just right) = Right right
