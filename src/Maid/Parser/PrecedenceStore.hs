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
)
where

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

data Associativity = LeftAssoc | RightAssoc
                   deriving Show

data Precedence = Precedence Integer Associativity
                deriving Show

newtype PrecMap = PrecMap (M.Map String Precedence)

mapPrecedence :: (Integer -> Integer) -> Precedence -> Precedence
mapPrecedence f (Precedence prec' assoc') = Precedence (f prec') assoc'

empty :: PrecMap
empty = PrecMap M.empty

fromList :: [(String, Precedence)] -> PrecMap
fromList = PrecMap . M.fromList

with :: String -> Precedence -> PrecMap -> PrecMap
with str precedence (PrecMap map') =
    PrecMap $ M.insert str precedence map'


get :: String -> PrecMap -> Maybe Precedence
get operator (PrecMap map') = M.lookup operator map'

getOr :: String -> Precedence -> PrecMap -> Precedence
getOr operator def (PrecMap map') =
    fromMaybe def (M.lookup operator map')
