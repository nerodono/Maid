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
, mapAssoc
, binaryOp
, unaryOp
)
where

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe ( fromMaybe )

data Associativity = LeftAssoc | RightAssoc
                   deriving(Show, Eq, Ord)

data Precedence = Precedence Integer Associativity
                deriving(Show, Eq, Ord)
data OperatorArity = ArBinary | ArUnary
                   deriving(Show, Eq, Ord)

data PrecMap = PrecMap (M.Map (OperatorArity, String) Precedence) (S.Set Integer)

mapPrecedence :: (Integer -> Integer) -> Precedence -> Precedence
mapPrecedence f (Precedence prec' assoc') = Precedence (f prec') assoc'

mapAssoc :: (Associativity -> Associativity) -> Precedence -> Precedence
mapAssoc f (Precedence prec' assoc') =
    Precedence prec' $ f assoc'

withArity :: OperatorArity -> String -> (OperatorArity, String)
withArity = (,)

binaryOp :: String -> (OperatorArity, String)
binaryOp = withArity ArBinary

unaryOp :: String -> (OperatorArity, String)
unaryOp = withArity ArUnary

empty :: PrecMap
empty = PrecMap M.empty S.empty

fromList :: [((OperatorArity, String), Precedence)] -> PrecMap
fromList l = PrecMap (M.fromList l) $ S.fromList precList
    where precList = extractPrec <$> l
          extractPrec (_, Precedence prec'' _) = prec''

with :: (OperatorArity, String) -> Precedence -> PrecMap -> PrecMap
with str precedence (PrecMap map' set') =
    PrecMap (M.insert str precedence map') (S.insert prec' set')
    where Precedence prec' _ = precedence

maxPrec :: PrecMap -> Integer
maxPrec (PrecMap _ set') = maximum set'

get :: (OperatorArity, String) -> PrecMap -> Maybe Precedence
get operator (PrecMap map' _) = M.lookup operator map'

getOr :: (OperatorArity, String) -> Precedence -> PrecMap -> Precedence
getOr operator def (PrecMap map' _) =
    fromMaybe def (M.lookup operator map')
