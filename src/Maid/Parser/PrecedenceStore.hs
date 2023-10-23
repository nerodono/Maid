module Maid.Parser.PrecedenceStore
( Associativity(..)
, Precedence(..)
, PrecMap
, empty
, fromList
, with
, get
, getOr
, mapPrecedence
, maxPrec
)
where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe ( fromMaybe )

data Associativity = LeftAssoc | RightAssoc
                   deriving(Show, Eq, Ord)

data Precedence = Precedence Integer Associativity
                deriving(Show, Eq, Ord)

data PrecMap = PrecMap (M.Map String Precedence) (S.Set Integer)

mapPrecedence :: (Integer -> Integer) -> Precedence -> Precedence
mapPrecedence f (Precedence prec' assoc') = Precedence (f prec') assoc'

empty :: PrecMap
empty = PrecMap M.empty S.empty

fromList :: [(String, Precedence)] -> PrecMap
fromList l = PrecMap (M.fromList l) $ S.fromList precList
    where precList = extractPrec <$> l
          extractPrec (_, Precedence prec'' _) = prec''

with :: String -> Precedence -> PrecMap -> PrecMap
with str precedence (PrecMap map' set') =
    PrecMap (M.insert str precedence map') (S.insert prec' set')
    where Precedence prec' _ = precedence

maxPrec :: PrecMap -> Integer
maxPrec (PrecMap _ set') = maximum set'

get :: String -> PrecMap -> Maybe Precedence
get operator (PrecMap map' _) = M.lookup operator map'

getOr :: String -> Precedence -> PrecMap -> Precedence
getOr operator def (PrecMap map' _) =
    fromMaybe def (M.lookup operator map')
