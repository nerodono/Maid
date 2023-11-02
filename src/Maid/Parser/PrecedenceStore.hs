module Maid.Parser.PrecedenceStore
( Store
, StoreTree
, Arity(..)
, Operator(..)
, ary
, unary
, binary
, fromList
, leftAssoc
, rightAssoc
, (<!!>)
, splitMin
, precedences
, fromRoot
, mapRoot
, splitTree
, root
)
where

import qualified Data.Bifunctor as Bi
import qualified Data.Map       as M

type Scope = M.Map Integer Integer

data Associativity = LeftAssoc | RightAssoc
                   deriving(Show, Eq, Ord)
newtype Arity      = Arity Integer
                   deriving(Show, Eq, Ord)
newtype Operator   = Operator (String, Arity)
                   deriving(Show, Eq, Ord)
data Precedence    = Precedence { associativity :: Associativity
                                , precedence    :: Integer
                                }
                   deriving(Show, Eq, Ord)

(<!!>) :: (Integer -> Integer) -> Precedence -> Precedence
(<!!>) f (Precedence assoc p) = Precedence assoc (f p)

leftAssoc :: Integer -> Precedence
leftAssoc = Precedence LeftAssoc

rightAssoc :: Integer -> Precedence
rightAssoc = Precedence RightAssoc

ary :: Integer -> String -> Operator
ary arity repr = Operator (repr, Arity arity)

unary :: String -> Operator
unary = ary 1

binary :: String -> Operator
binary = ary 2

data Store = Store { inner       :: M.Map Operator Precedence
                   , precedences :: Scope
                   }
           deriving Show
data StoreTree = StoreTree { root       :: Store
                           , scope      :: Scope } 

splitScope :: Scope -> Maybe (Integer, Scope)
splitScope scope =
    case M.keys scope of
        [] -> Nothing
        key:_ ->
            let scope'    = M.updateWithKey f key scope
                f _ v     = if v == 1 then Nothing else Just (v - 1)
            in
                Just (key, scope')

splitMin :: Store -> Maybe (Integer, Store)
splitMin (Store inner precedences) =
    Bi.second (Store inner) <$> splitScope precedences

fromRoot :: Store -> StoreTree
fromRoot (Store inner precedences) =
    StoreTree (Store inner precedences) precedences

mapRoot :: (Store -> Store) -> StoreTree -> StoreTree
mapRoot f (StoreTree root scope) =
    StoreTree (f root) scope

splitTree :: StoreTree -> Maybe (Integer, StoreTree)
splitTree (StoreTree root scope) =
    Bi.second (StoreTree root) <$> splitScope scope

fromList :: [(Operator, Precedence)] -> Store
fromList list' =
    let clamped     = Bi.second (max 0 <!!>) <$> list' -- clamp precedences to [0; +inf)
        inner       = M.fromList clamped               -- create mapping of precedences
        precedences = foldl f M.empty clamped          -- create index of precedences
        f m item    =
            let (_, Precedence _ p) = item
            in M.insertWith (+) p 1 m
    in
        Store inner precedences
